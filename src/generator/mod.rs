use std::{cell::RefCell, rc::Rc};

use crate::generator::{
    grammar_parser::{parse_grammar, validate_grammar},
    lexer::lex_grammar,
    parser::{generate_first, generate_follow, generate_slr_table, generate_states},
    types::{Production, State, Symbol, Term},
};

pub mod grammar_parser;
pub mod lexer;
pub mod parser;
pub mod types;

pub fn generate_parser(text: String) {
    let tokens = lex_grammar(text).unwrap();

    // Perform a top-down recursive descent parse of the grammar
    let mut grammar = parse_grammar(tokens);

    // Validate grammar (no duplicate productions, no duplicate terminals, etc.)
    validate_grammar(&grammar);

    // Generate the FIRST and FOLLOW sets
    let first = generate_first(&grammar);
    let follow = generate_follow(&grammar, &first);

    // Print the grammar section
    println!("====== Grammar =============");
    for prod in &grammar.grammar_section {
        println!("{}", prod);
    }
    println!("===========================\n");

    // Print the FIRST sets
    println!("===== FIRST sets ==========");
    for (term, set) in &first {
        print!("FIRST({}) = ", term);
        for sym in set {
            print!("{} ", sym);
        }
        println!("");
    }
    println!("===========================\n");

    // Print the FOLLOW sets
    println!("===== FOLLOW sets =========");
    for (term, set) in &follow {
        print!("FOLLOW({}) = ", term);
        for sym in set {
            print!("{} ", sym);
        }
        println!("");
    }
    println!("===========================\n");

    // Treat the first production as the start symbol
    let start_symbol = grammar.grammar_section[0].lhs.clone();
    let psuedo_start_symbol = format!("{}'", start_symbol);

    // Add a new production to the grammar with the psuedo start symbol (S' -> . S)
    grammar.nonterm_section.push(Term {
        str: psuedo_start_symbol.clone(),
    });
    grammar.grammar_section.insert(
        0,
        Production {
            lhs: psuedo_start_symbol.clone(),
            rhs: vec![Symbol {
                name: start_symbol.clone(),
                is_terminal: false,
            }],
            pos: 0,
        },
    );

    // Create a hashmap for storing the LR(0) states
    let states = Rc::new(RefCell::new(Vec::<State>::new()));

    // Generate closures for all states
    generate_states(&grammar, Rc::clone(&states));

    // Print the states
    let states = Rc::try_unwrap(states).unwrap().into_inner();
    for state in states.iter() {
        println!("{}", state);
    }

    // Create an augmented list of terminals with the EOF symbol without the epsilon symbol
    let mut term_index = grammar.term_section.clone();
    term_index.remove(term_index.len() - 1);
    term_index.push(Term {
        str: String::from("$"),
    });

    // Create a list of non-terminals
    let nonterm_index = grammar.nonterm_section.clone();

    // Create a numbered list of productions
    let prod_index = grammar.grammar_section.clone();

    // Generate the SLR parsing table
    let (action_table, goto_table) = generate_slr_table(&grammar, &states, &follow);

    // Print the grammar section
    println!("====== Grammar =============");
    for (i, prod) in grammar.grammar_section.iter().enumerate() {
        println!("{}: {}", i, prod);
    }

    println!("===========================\n");
    // Print the SLR parsing table
    println!("===== SLR parsing table ====");
    println!("Action table:");
    // Print header
    print!("      ");
    for term in &term_index {
        print!("{:<5.5} ", term.str);
    }
    println!("");
    for (state, row) in action_table.iter().enumerate() {
        print!("{:<4}| ", state);
        for action in row.iter() {
            match action {
                Some(action) => {
                    print!("{}", action.print_table(&prod_index));
                }
                None => print!("{} ", "-    "),
            }
        }
        println!("");
    }
    println!("\nGoto table:");
    // Print header
    print!("      ");
    for nonterm in &nonterm_index {
        print!("{:<5.5} ", nonterm.str);
    }
    println!("");
    for (state, row) in goto_table.iter().enumerate() {
        print!("{:<4}| ", state);
        for goto in row.iter() {
            match goto {
                Some(goto) => {
                    print!("{:<6}", goto);
                }
                None => print!("{} ", "-    "),
            }
        }
        println!("");
    }
    println!("===========================\n");

}
