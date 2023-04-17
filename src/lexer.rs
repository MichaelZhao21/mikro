use std::error::Error;

const BREAKING_CHARS: [char; 23] = [
    '(', ')', '{', '}', '[', ']', ',', ';', '+', '-', '*', '/', '%', '=', '!', '<', '>', '&', '|',
    '^', '~', '?', ':',
];

pub fn lex(text: String) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut tokens = Vec::new();

    // Get iterator over characters
    let mut chars = text.chars().peekable();

    // Get vector for characters
    let mut lex_string = LexString::new();

    // Loop over characters
    while let Some(c) = chars.next() {
        // If the current string is a string, check if the character is a quotation mark
        // If it is, push the string to the tokens and continue
        if lex_string.is_string {
            if c == '"' {
                tokens.push(Token::new_with_value(
                    TokenType::StringLit,
                    lex_string.done(),
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
                tokens.push(Token::new(lex_string.done()));
            }
            continue;
        }

        // Check if the character is a breaking character
        if BREAKING_CHARS.contains(&c) {
            if !lex_string.is_empty() {
                tokens.push(Token::new(lex_string.done()));
            }
            tokens.push(Token::new(c.to_string()));
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
    pub span: Span,
    pub value: Option<String>,
}

impl Token {
    pub fn new(str: String) -> Self {
        let token_type = match TokenType::new(str) {
            Ok(token_type) => token_type,
            Err(str) => {
                let (token_type, value) = TokenType::new_valued(str);
                return Self {
                    token_type,
                    span: Span { start: 0, end: 1 },
                    value: Some(value),
                }
            }
        };
        Self {
            token_type,
            span: Span { start: 0, end: 1 },
            value: None,
        }
    }

    pub fn new_with_value(token_type: TokenType, value: String) -> Self {
        Self {
            token_type,
            span: Span { start: 0, end: 1 },
            value: Some(value),
        }
    }
}

#[derive(Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
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
