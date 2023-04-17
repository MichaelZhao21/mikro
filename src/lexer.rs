use std::{error::Error, fmt};

const BREAKING_CHARS: [char; 23] = [
    '(', ')', '{', '}', '[', ']', ',', ';', '+', '-', '*', '/', '%', '=', '!', '<', '>', '&', '|',
    '^', '~', '?', ':',
];

pub fn lex(text: String) -> Result<Vec<Token>, Box<dyn Error>> {
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
            continue;
        }

        // Increment the column
        curr_loc.col += 1;

        // If the current string is a string, check if the character is a quotation mark
        // If it is, push the string to the tokens and continue
        if lex_string.is_string {
            if c == '"' {
                tokens.push(Token::new_with_value(
                    TokenType::StringLit,
                    lex_string.done(),
                    &curr_loc,
                ));
                continue;
            } else {
                lex_string.push(c);
                continue;
            }
        }

        // Check if the character is a quotation mark
        // If it is, set the is_string flag
        if c == '"' {
            lex_string.is_string = true;
            continue;
        }

        // Check if the character is whitespace
        if c.is_whitespace() {
            if !lex_string.is_empty() {
                tokens.push(Token::new(lex_string.done(), &curr_loc));
            }
            continue;
        }

        // Check if the character is a breaking character
        if BREAKING_CHARS.contains(&c) {
            if !lex_string.is_empty() {
                tokens.push(Token::new(lex_string.done(), &curr_loc));
            }
            tokens.push(Token::new(c.to_string(), &curr_loc));
            continue;
        }

        // Otherwise push the character to the current string
        lex_string.push(c);
    }

    Ok(tokens)
}

struct LexString {
    curr_str: Vec<char>,
    is_string: bool,
}

impl LexString {
    fn new() -> Self {
        Self {
            curr_str: Vec::new(),
            is_string: false,
        }
    }

    fn push(&mut self, c: char) {
        self.curr_str.push(c);
    }

    fn done(&mut self) -> String {
        let s = self.curr_str.iter().collect();

        // Clear the current string
        self.curr_str.clear();
        self.is_string = false;

        // Return the string
        s
    }

    fn is_empty(&self) -> bool {
        self.curr_str.is_empty()
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub loc: Loc,
    pub value: Option<String>,
}

impl Token {
    pub fn new(str: String, loc: &Loc) -> Self {
        let token_type = match TokenType::new(str) {
            Ok(token_type) => token_type,
            Err(str) => {
                let (token_type, value) = TokenType::new_valued(str);
                return Self {
                    token_type,
                    loc: loc.clone(),
                    value: Some(value),
                };
            }
        };
        Self {
            token_type,
            loc: loc.clone(),
            value: None,
        }
    }

    pub fn new_with_value(token_type: TokenType, value: String, loc: &Loc) -> Self {
        Self {
            token_type,
            loc: loc.clone(),
            value: Some(value),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{} {:?}({})", self.loc, self.token_type, value),
            None => write!(f, "{} {:?}", self.loc, self.token_type),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Loc {
    pub row: usize,
    pub col: usize,
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}, {}]", self.row, self.col)
    }
}

/// The type of a token (eg. class, function, IntLit, etc.)
#[derive(Debug)]
pub enum TokenType {
    // Types without values
    Function,
    Return,
    Aiya, // Print
    LParen,
    RParen,
    LCurly,
    RCurly,
    LBracket,
    RBracket,
    Comma,
    Semicolon,
    Colon,

    // Types with values
    Type,
    Id,
    StringLit,
    IntLit,
    FloatLit,
    BoolLit,
}

impl TokenType {
    pub fn new(str: String) -> Result<Self, String> {
        match str.as_str() {
            "func" => Ok(Self::Function),
            "return" => Ok(Self::Return),
            "aiya" => Ok(Self::Aiya),
            "(" => Ok(Self::LParen),
            ")" => Ok(Self::RParen),
            "{" => Ok(Self::LCurly),
            "}" => Ok(Self::RCurly),
            "[" => Ok(Self::LBracket),
            "]" => Ok(Self::RBracket),
            "," => Ok(Self::Comma),
            ";" => Ok(Self::Semicolon),
            ":" => Ok(Self::Colon),
            _ => Err(str),
        }
    }

    pub fn new_valued(str: String) -> (Self, String) {
        // Check if the string only contains digits
        if str.chars().all(|c| c.is_digit(10)) {
            return (Self::IntLit, str);
        }

        // Check if the string is a type
        if str == "int" || str == "float" || str == "bool" || str == "string" {
            return (Self::Type, str);
        }

        // TODO: Check if the string is a float, bool, or type

        // Otherwise, return the string as an identifier
        (Self::Id, str)
    }
}
