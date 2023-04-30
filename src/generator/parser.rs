use std::{cell::RefCell, collections::HashSet, rc::Rc};

use super::types::{Grammar, Production, State, Symbol};

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
