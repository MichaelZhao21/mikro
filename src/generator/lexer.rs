use crate::lexer::{LexString, Loc};

use super::types::GeneratorToken;

const BREAKING_CHARS: [char; 2] = ['|', ';'];

pub fn lex_grammar(text: String) -> Result<Vec<GeneratorToken>, String> {
    let mut tokens = Vec::new();

    // Get iterator over characters
    let mut chars = text.chars().peekable();

    // Create vector for forming strings
    let mut lex_string = LexString::new();

    // Create variable for keeping track of the current location
    let mut curr_loc = Loc { row: 1, col: 0 };

    // Loop over characters
    while let Some(c) = chars.next() {
        // If the character is a newline, increment the row and reset the column
        if c == '\n' {
            curr_loc.row += 1;
            curr_loc.col = 0;
        }

        // Increment the column
        curr_loc.col += 1;

        // Check if the character is a quotation mark
        // If it is, set the is_string flag
        if c == '"' {
            lex_string.is_string = true;
            continue;
        }

        // Check if the character is whitespace
        if c.is_whitespace() {
            if !lex_string.is_empty() {
                tokens.push(GeneratorToken::new(lex_string.done(), &curr_loc));
            }
            continue;
        }

        // Check if the character is a breaking character
        if BREAKING_CHARS.contains(&c) {
            if !lex_string.is_empty() {
                tokens.push(GeneratorToken::new(lex_string.done(), &curr_loc));
            }
            tokens.push(GeneratorToken::new(c.to_string(), &curr_loc));
            continue;
        }

        // Otherwise push the character to the current string
        lex_string.push(c);
    }

    Ok(tokens)
}
