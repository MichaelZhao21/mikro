use generator::Parser;
use mikro::lexer;

#[derive(Parser)]
#[grammar = "examples/grammars/dragon.mik"]
struct DragonParser;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();

    // Make sure there is one argument (input file)
    if args.len() != 2 {
        eprintln!("Usage: {} <input file>", args[0]);
        return;
    }

    // Read the input file
    let text = std::fs::read_to_string(&args[1]).unwrap();

    // Call the lexer
    let tokens = lexer::lex(text).unwrap();
    
    // Print the tokens
    for token in tokens {
        println!("{}", token);
    }

    // Print out static TERM_INDEX
    println!("TERM_INDEX:");
    for (i, term) in DragonParser::TERM_INDEX.iter().enumerate() {
        println!("\t{} {}", i, term);
    }

    // Print out static NONTERM_INDEX
    println!("NONTERM_INDEX:");
    for (i, term) in DragonParser::NONTERM_INDEX.iter().enumerate() {
        println!("\t{} {}", i + DragonParser::NONTERM_OFFSET, term);
    }

    // Print out static PROD_INDEX
    for term in DragonParser::PROD_INDEX.iter() {
        println!("    {:?}", term);
    }

    let p = Production::decode_row(&DragonParser::PROD_INDEX[3]);
    println!("{}", p);
}
