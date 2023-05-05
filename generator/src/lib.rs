extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use std::{cell::RefCell, fs, rc::Rc};
use syn::{Attribute, DeriveInput};
use types::Action;

use crate::{
    grammar_parser::{parse_grammar, validate_grammar},
    lexer::lex_grammar,
    parser::{generate_first, generate_follow, generate_slr_table, generate_states},
    types::{Production, State, Symbol, Term},
};

mod grammar_parser;
mod lexer;
mod parser;
mod types;

#[proc_macro_derive(Parser, attributes(grammar))]
pub fn parser_macro(input: TokenStream) -> TokenStream {
    // Get input_file derive custom attribute
    let ast: DeriveInput = syn::parse2(input.into()).unwrap();
    let filenames: Vec<&Attribute> = ast
        .attrs
        .iter()
        .filter(|attr| {
            let path = attr.meta.path();
            path.is_ident("grammar")
        })
        .collect();

    // Make sure there's only 1 filename
    if filenames.len() != 1 {
        panic!("Must have exactly 1 grammar attribute");
    }

    // Get the filename
    let filename = match filenames[0]
        .meta
        .require_name_value()
        .unwrap()
        .value
        .clone()
    {
        syn::Expr::Lit(s) => match s.lit {
            syn::Lit::Str(s) => s.value(),
            _ => panic!("grammar attribute must be a string"),
        },
        _ => panic!("grammar attribute must be a string"),
    };

    // Read the file and get the name from the file
    let text = fs::read_to_string(&filename).unwrap();

    // Generate the parser
    let (term_index, nonterm_index, prod_index, action_table, goto_table) = generate_parser(text);

    // Convert the terminal/nonterminal indices to strings
    let term_index = term_index.iter().map(|t| t.str.clone()).collect::<Vec<_>>();
    let nonterm_index = nonterm_index
        .iter()
        .map(|t| t.str.clone())
        .collect::<Vec<_>>();

    // Get the maximum production length (+1 for count and LHS)
    let max_prod_len = prod_index
        .iter()
        .map(|p| p.rhs.len())
        .max()
        .unwrap_or_default()
        + 2;

    // Generate production index using production to token row function
    let prod_index = prod_index
        .iter()
        .map(|p| {
            production_to_token_row(
                p,
                &term_index,
                &nonterm_index,
                term_index.len(),
                max_prod_len,
            )
        })
        .collect::<Vec<_>>();

    // Get count variables for the output tables
    let term_count = term_index.len();
    let nonterm_count = nonterm_index.len();
    let prod_count = prod_index.len();

    // Generate the output
    let name = &ast.ident;

    let out = quote! {
        // Trait for associated consts
        trait ParserTables {
            const NONTERM_OFFSET: usize;
            const TERM_INDEX: [&'static str; #term_count];
            const NONTERM_INDEX: [&'static str; #nonterm_count];
            const PROD_INDEX: [[i32; #max_prod_len]; #prod_count];
            // const GOTO_TABLE: &'static [[Option<usize>]];
            // const ACTION_TABLE: &'static [[Option<Action>]];
        }

        // Generate a static array for the term, nonterm, and prod indices
        // As well as the GOTO and ACTION tables
        impl ParserTables for #name {
            const NONTERM_OFFSET: usize = #term_count;
            const TERM_INDEX: [&'static str; #term_count] = [#(#term_index),*];
            const NONTERM_INDEX: [&'static str; #nonterm_count] = [#(#nonterm_index),*];
            const PROD_INDEX: [[i32; #max_prod_len]; #prod_count] = [#(#prod_index),*];
            // const GOTO_TABLE: &'static [[Option<usize>]] = &GOTO_TABLE;
            // const ACTION_TABLE: &'static [[Option<Action>]] = &ACTION_TABLE;
        }

        // Generate a struct for the productions
        #[derive(Debug)]
        pub struct Production {
            pub lhs: String,
            pub rhs: Vec<String>,
        }

        impl Production {
            pub fn new(lhs: String, rhs: Vec<String>) -> Self {
                Self { lhs, rhs }
            }

            pub fn decode_row(row: &[i32; #max_prod_len]) -> Self {
                let len = row[0] as usize;
                let lhs = row[1] as usize;
                let rhs = row[2..len + 2].iter().map(|x| *x as usize).collect::<Vec<_>>();
                Self {
                    lhs: #name::NONTERM_INDEX[lhs - #name::NONTERM_OFFSET].to_string(),
                    rhs: rhs.iter().map(|x| {
                        if *x < #name::NONTERM_OFFSET {
                            #name::TERM_INDEX[*x].to_string()
                        } else {
                            #name::NONTERM_INDEX[*x - #name::NONTERM_OFFSET].to_string()
                        }
                    }).collect::<Vec<_>>(),
                }
            }
        }

        impl std::fmt::Display for Production {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{} ->", self.lhs)?;
                for sym in &self.rhs {
                    write!(f, " {}", sym)?;
                }
                Ok(())
            }
        }
    };

    out.into()
}

fn generate_parser(
    text: String,
) -> (
    Vec<Term>,
    Vec<Term>,
    Vec<Production>,
    Vec<Vec<Option<Action>>>,
    Vec<Vec<Option<usize>>>,
) {
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

    // Return the index vectors and the parsing tables
    (
        term_index,
        nonterm_index,
        prod_index,
        action_table,
        goto_table,
    )
}

fn production_to_token_row(
    prod: &Production,
    term_index: &Vec<String>,
    nonterm_index: &Vec<String>,
    nonterm_offset: usize,
    max_prod_len: usize,
) -> proc_macro2::TokenStream {
    // Map LHS to index
    let lhs = map_to_index(
        term_index,
        nonterm_index,
        nonterm_offset,
        &Symbol::new(prod.lhs.clone(), false),
    );

    // Map RHS symbols to their indices
    let rhs = prod
        .rhs
        .clone()
        .iter()
        .map(|sym| map_to_index(term_index, nonterm_index, nonterm_offset, sym))
        .collect::<Vec<i32>>();

    // Get length of RHS
    let len = rhs.len() as i32;

    // Pad remaining space with -1s
    let rem = vec![-1; max_prod_len - rhs.len() - 2];

    quote! {
        [#len,#lhs,#(#rhs),*,#(#rem),*]
    }
}

fn map_to_index(
    term_index: &Vec<String>,
    nonterm_index: &Vec<String>,
    nonterm_offset: usize,
    sym: &Symbol,
) -> i32 {
    let x = if sym.is_terminal {
        term_index.iter().position(|t| t == &sym.name).unwrap()
    } else {
        nonterm_index.iter().position(|n| n == &sym.name).unwrap() + nonterm_offset
    };

    x as i32
}
