use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use super::types::{Action, ActionType, Grammar, Production, State, Symbol};

pub fn generate_states(grammar: &Grammar, states: Rc<RefCell<Vec<State>>>) {
    // Get the closure for the first state
    states.borrow_mut().insert(
        0,
        State::new(0, grammar.grammar_section[0].clone(), grammar),
    );

    // Keeps track of already generated transition symbols
    let mut sym_set = Vec::<HashSet<Symbol>>::new();

    // Add the first state to the symbol set
    sym_set.push(HashSet::<Symbol>::new());

    let mut changed = true;
    while changed {
        changed = false;

        // Keeps track of new states added
        let mut new_states = Vec::<State>::new();
        let mut next_state_num = states.borrow_mut().len();

        // Iterate through the states
        let state_count = states.borrow().len();
        for i in 0..state_count {
            // Clone the list of productions
            let prods = states.borrow()[i].productions.clone();

            // Iterate through the productions in the given state
            let prod_count = prods.len();
            for j in 0..prod_count {
                let prod = &prods[j];

                // Make sure production is not in a final state or already has a transition
                if prod.is_complete()
                    || (sym_set.len() > i && sym_set[i].contains(&prod.next_sym().unwrap()))
                {
                    continue;
                }

                // Get the next symbol
                let sym = prod.next_sym().unwrap();

                // Add it to the symbol set
                sym_set[i].insert(sym.clone());

                // Get the updated production and advance the dot
                let mut new_prod = prod.clone();
                new_prod.advance();

                // Get the state that the new production belongs to
                let state_num = match get_state_from_prod(&states.borrow(), &new_prod) {
                    Some(num) => num,
                    None => {
                        // If the state does not exist, create it
                        let new_state = State::new(next_state_num, new_prod, grammar);
                        new_states.push(new_state);
                        sym_set.push(HashSet::new());
                        next_state_num += 1;
                        next_state_num - 1
                    }
                };

                // Add the transition to the current state
                states.borrow_mut()[i].add_transition(sym, state_num);

                changed = true;
            }

            // Add the new states to the states vector
            states.borrow_mut().append(&mut new_states);
            new_states.clear();
        }
    }
}

pub fn get_state_from_prod(states: &Vec<State>, prod: &Production) -> Option<usize> {
    for (num, state) in states.iter().enumerate() {
        if state.productions.contains(prod) {
            return Some(num.clone());
        }
    }
    None
}

pub fn generate_first(grammar: &Grammar) -> HashMap<String, HashSet<Symbol>> {
    let mut first_set = HashMap::<String, HashSet<Symbol>>::new();

    // Add all terminals to the FIRST set, which is just the terminal itself
    for term in &grammar.term_section {
        first_set.insert(term.str.clone(), HashSet::new());
        first_set
            .get_mut(&term.str)
            .unwrap()
            .insert(Symbol::new(term.str.clone(), true));
    }

    // Add all nonterminals to the FIRST set
    for term in &grammar.nonterm_section {
        first_set.insert(term.str.clone(), HashSet::new());
    }

    // Repeat until no more changes are made
    let mut changed = true;
    while changed {
        changed = false;

        // Iterate through the productions
        for prod in &grammar.grammar_section {
            // Check if the production derives empty
            if prod.is_complete() {
                if first_set.get(&prod.lhs).unwrap().contains(&Symbol::empty()) {
                    continue;
                } else {
                    first_set
                        .get_mut(&prod.lhs)
                        .unwrap()
                        .insert(Symbol::empty());
                    changed = true;
                }
            }

            // Flag to see if we broke out of the loop early
            let mut empty_flag = true;

            // Iterate through the symbols in the production until we hit a terminal
            // or the end of the production
            for sym in prod.rhs.iter() {
                // Check if the FIRST of the current symbol is a subset of the FIRST of the LHS's symbol
                if !is_subset(
                    first_set.get(&sym.name).unwrap(),
                    first_set.get(&prod.lhs).unwrap(),
                ) {
                    // If not, add the FIRST of the current symbol to the FIRST of the LHS's symbol
                    let first_sym = first_set.get(&sym.name).unwrap().clone();
                    first_set.get_mut(&prod.lhs).unwrap().extend(first_sym);

                    // Check if the current symbol derives empty
                    changed = true;
                }

                // Check if the current symbol derives empty
                // If not, break out of the loop
                if !first_set.get(&sym.name).unwrap().contains(&Symbol::empty()) {
                    empty_flag = false;
                    break;
                }
            }

            // If we did not break out of the loop, that means that all symbols derive empty
            // Add empty to the FIRST of the LHS's symbol
            if empty_flag {
                // Make sure empty is not already in the FIRST set
                if !first_set.get(&prod.lhs).unwrap().contains(&Symbol::empty()) {
                    first_set
                        .get_mut(&prod.lhs)
                        .unwrap()
                        .insert(Symbol::empty());
                    changed = true;
                }
            }
        }
    }

    first_set
}

pub fn is_subset(sub_set: &HashSet<Symbol>, super_set: &HashSet<Symbol>) -> bool {
    for sym in sub_set {
        if !super_set.contains(sym) {
            return false;
        }
    }
    true
}

pub fn generate_follow(
    grammar: &Grammar,
    first_set: &HashMap<String, HashSet<Symbol>>,
) -> HashMap<String, HashSet<Symbol>> {
    let mut follow_set = HashMap::<String, HashSet<Symbol>>::new();

    // Add all nonterminals to the FOLLOW set
    for term in &grammar.nonterm_section {
        follow_set.insert(term.str.clone(), HashSet::new());
    }

    // Place EOF in the follow set of the start symbol
    follow_set
        .get_mut(&grammar.grammar_section[0].lhs)
        .unwrap()
        .insert(Symbol::eof());

    // Repeat until no more changes are made
    let mut changed = true;
    while changed {
        changed = false;

        // Iterate through the productions
        for prod in &grammar.grammar_section {
            let prod_size = prod.rhs.len();

            // Ignore empty productions
            if prod_size == 0 {
                continue;
            }

            // Iterate through the symbols in the production
            for i in 0..(prod_size - 1) {
                // If the current symbol is a terminal, skip it
                if prod.rhs[i].is_terminal {
                    continue;
                }

                // Check if the FIRST set of the next symbol is a subset of the FOLLOW set of the current symbol
                if !is_subset(
                    first_set.get(&prod.rhs[i + 1].name).unwrap(),
                    follow_set.get(&prod.rhs[i].name).unwrap(),
                ) {
                    // If not, add the FIRST set of the next symbol to the FOLLOW set of the current symbol
                    let next_first_set = first_set.get(&prod.rhs[i + 1].name).unwrap().clone();
                    follow_set
                        .get_mut(&prod.rhs[i].name)
                        .unwrap()
                        .extend(next_first_set);

                    // Check if the next symbol derives empty
                    changed = true;
                }
            }

            // Iterate through the symbols in the production in reverse order!
            for i in (0..prod_size).rev() {
                // Break if the current symbol is a terminal
                if prod.rhs[i].is_terminal {
                    break;
                }

                // Check if FOLLOW set of the LHS is a subset of the FOLLOW set of the current symbol
                if !is_subset(
                    follow_set.get(&prod.lhs).unwrap(),
                    follow_set.get(&prod.rhs[i].name).unwrap(),
                ) {
                    // If not, add the FOLLOW set of the LHS to the FOLLOW set of the current symbol
                    let lhs_follow_set = follow_set.get(&prod.lhs).unwrap().clone();
                    follow_set
                        .get_mut(&prod.rhs[i].name)
                        .unwrap()
                        .extend(lhs_follow_set.clone());

                    // Check if the next symbol derives empty
                    changed = true;
                }

                // If the current symbol does not derive empty, break
                if !first_set
                    .get(&prod.rhs[i].name)
                    .unwrap()
                    .contains(&Symbol::empty())
                {
                    break;
                }
            }
        }
    }

    // Iterate through the FOLLOW sets
    for (_, set) in follow_set.iter_mut() {
        // Remove empty from the FOLLOW set
        set.remove(&Symbol::empty());
    }

    follow_set
}

pub fn generate_slr_table(
    grammar: &Grammar,
    states: &Vec<State>,
    first: &HashMap<String, HashSet<Symbol>>,
    follow: &HashMap<String, HashSet<Symbol>>,
) -> (Vec<Vec<Option<Action>>>, Vec<Vec<Option<usize>>>) {
    // Create the ACTION table
    let mut action_table =
        vec![vec![None as Option<Action>; grammar.term_section.len() + 1]; states.len()];

    // Create the GOTO table
    let mut goto_table =
        vec![vec![None as Option<usize>; grammar.nonterm_section.len()]; states.len()];

    // Get the pseudo start symbol
    let pseudo_start = grammar.grammar_section[0].lhs.clone();

    // Iterate through the states
    for (i, state) in states.iter().enumerate() {
        // Iterate through the productions in the state
        for prod in &state.productions {
            // Check if the production is complete
            if prod.is_complete() {
                // If this is the start state, accept
                if prod.lhs == pseudo_start {
                    action_table[i][grammar.term_section.len()] = Some(Action::accept());
                } else {
                    // Otherwise, reduce
                    for term in &follow[&prod.lhs] {
                        let j = get_term_index(&term.name, grammar);

                        // Create the new reduce action
                        let reduce_action = Action::reduce(prod.clone());

                        // Check to make sure there is no conflict
                        if action_table[i][j].is_some()
                            && has_conflict(action_table[i][j].as_ref().unwrap(), &reduce_action)
                        {
                            let curr_action = action_table[i][j].as_ref().unwrap();
                            print_conflict(
                                i,
                                &term.name,
                                &curr_action.action_type,
                                &ActionType::Reduce,
                            );
                        }

                        // Add the reduce action
                        action_table[i][j] = Some(Action::reduce(prod.clone()));
                    }
                }

                // Continue to the next production
                continue;
            }

            // If the next symbol is a nonterminal, ignore
            if !prod.next_sym().unwrap().is_terminal {
                continue;
            }

            // Get the index of the next symbol
            let j = get_term_index(&prod.next_sym().unwrap().name, grammar);

            // Create the new shift action
            let shift_action = Action::shift(state.get_next_state(prod));

            // Check to make sure there is no conflict
            if action_table[i][j].is_some()
                && has_conflict(action_table[i][j].as_ref().unwrap(), &shift_action)
            {
                let curr_action = action_table[i][j].as_ref().unwrap();
                print_conflict(
                    i,
                    &prod.next_sym().unwrap().name,
                    &curr_action.action_type,
                    &ActionType::Shift,
                );
            }

            // Add the shift action
            action_table[i][j] = Some(shift_action);
        }
    }

    // Return the tables
    (action_table, goto_table)
}

pub fn get_term_index(term: &String, grammar: &Grammar) -> usize {
    // Check if the term is the EOF symbol
    if term == "$" {
        return grammar.term_section.len();
    }

    // Iterate through the terms
    for (i, t) in grammar.term_section.iter().enumerate() {
        if t.str == *term {
            return i;
        }
    }

    // If the term is not found, panic
    panic!("{:?} - Term not found", term);
}

pub fn has_conflict(action_1: &Action, action_2: &Action) -> bool {
    // Check if the actions types are the same and if not return true
    if action_1.action_type != action_2.action_type {
        return true;
    }

    // Check if the actions are both shifts
    if action_1.action_type == ActionType::Shift {
        // If so, check if the states are the same and if not return true
        if action_1.shift_state != action_2.shift_state {
            return true;
        }
    }

    // Check if the actions are both reduces
    if action_1.action_type == ActionType::Reduce {
        // If so, check if the productions are the same and if not return true
        if action_1.reduce_prod != action_2.reduce_prod {
            return true;
        }
    }

    // If no conflicts are found, return false
    false
}

pub fn print_conflict(
    state: usize,
    term: &String,
    action_type_1: &ActionType,
    action_type_2: &ActionType,
) {
    panic!(
        "{}/{} conflict at state {} on term {}",
        action_type_1, action_type_2, state, term
    );
}
