use std::{cell::RefCell, rc::Rc};

use crate::generator::{
    grammar_parser::{parse_grammar, validate_grammar},
    lexer::lex_grammar,
    parser::{generate_states, generate_first, generate_follow},
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

    // Unwrap the states
    let states = Rc::try_unwrap(states).unwrap().into_inner();

    for state in states.iter() {
        println!("{}", state);
    }
}
