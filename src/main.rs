use mikro::lexer;

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
        println!("{:?}", token);
    }
}
