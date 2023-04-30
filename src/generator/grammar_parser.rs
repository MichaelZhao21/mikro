use crate::lexer::Loc;

use super::types::{
    GeneratorToken, GeneratorTokenType, Grammar, Parseable, Production, TokenStream,
};

pub fn parse_grammar(tokens: Vec<GeneratorToken>) -> Grammar {
    // Add EOF token
    let mut tokens = tokens;
    tokens.push(GeneratorToken {
        token_type: GeneratorTokenType::EOF,
        value: None,
        loc: Loc { row: 0, col: 0 },
    });

    // Create a token stream
    let mut stream = TokenStream::new(tokens);

    // Parse the grammar
    Grammar::parse(&mut stream)
}

pub fn validate_grammar(grammar: &Grammar) {
    // Check for duplicate terminals
    let mut terminals = Vec::<String>::new();
    for term in &grammar.term_section {
        if terminals.contains(&term.str) {
            panic!("Duplicate terminal: {}", term.str);
        }
        terminals.push(term.str.clone());
    }

    // Check for duplicate nonterminals
    let mut nonterminals = Vec::<String>::new();
    for term in &grammar.nonterm_section {
        if nonterminals.contains(&term.str) {
            panic!("Duplicate nonterminal: {}", term.str);
        }
        nonterminals.push(term.str.clone());
    }

    // Check for duplicate productions
    let mut productions = Vec::<&Production>::new();
    for prod in &grammar.grammar_section {
        if productions.contains(&prod) {
            panic!("Duplicate production: {:?}", prod);
        }
        productions.push(prod);
    }

    // Check to make sure all terminals and nonterminals in the productions are defined
    for prod in &grammar.grammar_section {
        for sym in &prod.rhs {
            if sym.is_terminal {
                if !terminals.contains(&sym.name) {
                    panic!("Undefined terminal: {}", sym.name);
                }
            } else {
                if !nonterminals.contains(&sym.name) {
                    panic!("Undefined nonterminal: {}", sym.name);
                }
            }
        }
    }
}
