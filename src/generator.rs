use std::{
    collections::{HashMap, HashSet},
    fmt,
};

use crate::lexer::{LexString, Loc};

pub fn generate_parser(text: String) {
    let tokens = lex_grammar(text).unwrap();

    // Perform a top-down recursive descent parse of the grammar
    let mut grammar = parse_grammar(tokens);

    // Validate grammar (no duplicate productions, no duplicate terminals, etc.)
    validate_grammar(&grammar);

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
    let mut states = Vec::<State>::new();

    // Generate closures for all states
    generate_states(grammar, &mut states);

    println!("{:#?}", states);
}

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

fn parse_grammar(tokens: Vec<GeneratorToken>) -> Grammar {
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

pub fn generate_states(grammar: Grammar, states: &mut Vec<State>) {
    // Get the closure for the first state
    states.insert(0, State::new(0, grammar.grammar_section[0].clone()));
    closure(grammar, states, 0);

    let mut changed = true;
    while changed {
        changed = false;

        // Keeps track of already generated transition symbols
        let mut sym_set = HashSet::<&Symbol>::new();

        // Keeps track of new states added
        let mut new_states = HashMap::<usize, State>::new();
        let next_state_num = states.len();

        // Iterate through the states
        for state in states.iter_mut() {
            // Iterate through the productions in the given state
            for prod in &state.productions {
                // Make sure production is not in a final state or already has a transition
                if prod.is_complete() || sym_set.contains(&prod.next_sym().unwrap()) {
                    continue;
                }

                // Get the next symbol
                let sym = prod.next_sym().unwrap();

                // Add it to the set
                sym_set.insert(sym);

                // Get the updated production and advance the dot
                let mut new_prod = prod.clone();
                new_prod.advance();

                // Get the state that the new production belongs to
                let state_num = match get_state_from_prod(states, &new_prod) {
                    Some(num) => num,
                    None => {
                        // If the state does not exist, create it
                        let num = next_state_num + new_states.len();

                        // Add the state to the new states
                        new_states.insert(num, State::new(num, new_prod.clone()));

                        // Get the state number
                        num
                    }
                };
                
                // Add transition to the current state
                state.add_transition(&sym, state_num);
            }
        }
    }
}

pub fn closure(grammar: Grammar, states: &mut Vec<State>, state: usize) {
    // Check to make sure state already exists and has at least one item
    if states.len() <= state {
        panic!("State {} does not exist", state);
    }
    if states.get(state).unwrap().productions.len() == 0 {
        panic!("State {} has no items", state);
    }

    // Get the current state
    let curr_state = states.get_mut(state).unwrap();

    // Loop until nothing changes
    let mut changed = true;
    while changed {
        changed = false;

        // Vector of new productions to add
        let mut new_prods = Vec::<Production>::new();

        // Loop over the productions in the state
        for prod in &curr_state.productions {
            // Make sure the production is not complete
            if prod.is_complete() {
                continue;
            }

            // Get the next symbol
            let sym = prod.next_sym().unwrap();
            if sym.is_terminal {
                continue;
            }

            // Loop through the grammar productions
            for g_prod in &grammar.grammar_section {
                // Check if the production matches the next symbol
                if g_prod.lhs != sym.name {
                    continue;
                }

                // Check if production is already in the state
                if curr_state.productions.contains(&g_prod) {
                    continue;
                }

                // Add the production to the state
                new_prods.push(g_prod.clone());

                // Set changed flag
                changed = true;
            }
        }

        // Append the new productions to the state
        curr_state.add_productions(new_prods.clone());
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

struct TokenStream {
    tokens: Vec<GeneratorToken>,
    index: usize,
}

impl TokenStream {
    fn new(tokens: Vec<GeneratorToken>) -> Self {
        Self { tokens, index: 0 }
    }

    fn peek(&self) -> &GeneratorToken {
        self.tokens.get(self.index).unwrap()
    }

    fn next(&mut self) -> &GeneratorToken {
        let token = self.tokens.get(self.index).unwrap();
        self.index += 1;
        token
    }
}

trait Parseable {
    fn parse(stream: &mut TokenStream) -> Self;
}

pub struct Grammar {
    pub term_section: Vec<Term>,
    pub nonterm_section: Vec<Term>,
    pub grammar_section: Vec<Production>,
}

impl Parseable for Grammar {
    fn parse(stream: &mut TokenStream) -> Self {
        let mut term_section = Vec::<Term>::new();
        let mut nonterm_section = Vec::<Term>::new();
        let mut grammar_section = Vec::<Production>::new();

        // Check for divider then tokens token
        if stream.next().token_type != GeneratorTokenType::Divider {
            panic!("Expected divider");
        }
        if stream.next().token_type != GeneratorTokenType::TerminalLabel {
            panic!("Expected terminal label");
        }

        // Parse terminal section
        while stream.peek().token_type != GeneratorTokenType::Divider {
            term_section.push(Term::parse(stream));
        }

        // Check for divider then nonterminal token
        if stream.next().token_type != GeneratorTokenType::Divider {
            panic!("Expected divider");
        }
        if stream.next().token_type != GeneratorTokenType::NonTerminalLabel {
            panic!("Expected nonterminal label");
        }

        // Parse nonterminal section
        while stream.peek().token_type != GeneratorTokenType::Divider {
            nonterm_section.push(Term::parse(stream));
        }

        // Check for divider then grammar token
        if stream.next().token_type != GeneratorTokenType::Divider {
            panic!("Expected divider");
        }
        if stream.next().token_type != GeneratorTokenType::GrammarLabel {
            panic!("Expected grammar label");
        }

        // Parse grammar section
        while stream.peek().token_type != GeneratorTokenType::EOF {
            grammar_section.push(Production::parse(stream));
        }

        // Add epsilon symbol to terminal section
        term_section.push(Term {
            str: "e".to_string(),
        });

        Self {
            term_section,
            nonterm_section,
            grammar_section,
        }
    }
}

#[derive(Debug)]
pub struct Term {
    pub str: String,
}

impl Parseable for Term {
    fn parse(stream: &mut TokenStream) -> Self {
        let token = stream.next();
        if token.token_type != GeneratorTokenType::Id {
            panic!("Expected id");
        }
        Self {
            str: token.value.clone().unwrap(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Production {
    pub lhs: String,
    pub rhs: Vec<Symbol>,
    // Position of the dot in the rule, i.e. S -> a . b c (where pos = 1)
    // If the dot is at the end of the rule, pos = rhs.len()
    pub pos: usize,
}

impl Production {
    pub fn new(lhs: String, rhs: Vec<Symbol>) -> Self {
        Self { lhs, rhs, pos: 0 }
    }

    pub fn next_sym(&self) -> Option<&Symbol> {
        self.rhs.get(self.pos)
    }

    pub fn is_complete(&self) -> bool {
        self.pos == self.rhs.len()
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }
}

impl Parseable for Production {
    fn parse(stream: &mut TokenStream) -> Self {
        // Sanity check
        if stream.peek().token_type == GeneratorTokenType::EOF {
            panic!("Unexpected EOF");
        }

        // Get the lhs
        let lhs = stream.next().value.clone().unwrap();

        // Check for arrow
        if stream.next().token_type != GeneratorTokenType::Arrow {
            panic!("Expected arrow");
        }

        // Get the rhs
        let mut rhs = Vec::<Symbol>::new();

        // Pipes will split the rhs into multiple productions
        while stream.peek().token_type != GeneratorTokenType::Semi
            && stream.peek().token_type != GeneratorTokenType::EOF
        {
            while stream.peek().token_type != GeneratorTokenType::Pipe
                && stream.peek().token_type != GeneratorTokenType::Semi
                && stream.peek().token_type != GeneratorTokenType::EOF
            {
                rhs.push(Symbol::parse(stream));
            }
            if stream.peek().token_type == GeneratorTokenType::Pipe {
                stream.next();
            }
        }

        // Check for semicolon
        if stream.next().token_type != GeneratorTokenType::Semi {
            panic!("Expected semicolon");
        }

        Self { lhs, rhs, pos: 0 }
    }
}

#[derive(Debug, Clone)]
pub struct GeneratorToken {
    pub token_type: GeneratorTokenType,
    pub value: Option<String>,
    pub loc: Loc,
}

impl GeneratorToken {
    pub fn new(str: String, loc: &Loc) -> Self {
        let token_type = match GeneratorTokenType::new(str) {
            Ok(token_type) => token_type,
            Err(str) => {
                let (token_type, value) = GeneratorTokenType::new_valued(str);
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

    pub fn new_with_value(token_type: GeneratorTokenType, value: String, loc: &Loc) -> Self {
        Self {
            token_type,
            loc: loc.clone(),
            value: Some(value),
        }
    }
}

impl fmt::Display for GeneratorToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.value {
            Some(value) => write!(f, "{} {:?}({})", self.loc, self.token_type, value),
            None => write!(f, "{} {:?}", self.loc, self.token_type),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GeneratorTokenType {
    Divider,
    Arrow,
    Pipe,
    Semi,
    TerminalLabel,
    NonTerminalLabel,
    GrammarLabel,
    Id,
    EOF,
}

impl GeneratorTokenType {
    pub fn new(str: String) -> Result<Self, String> {
        match str.as_str() {
            "=====" => Ok(Self::Divider),
            "=>" => Ok(Self::Arrow),
            "|" => Ok(Self::Pipe),
            ";" => Ok(Self::Semi),
            "t" => Ok(Self::TerminalLabel),
            "nt" => Ok(Self::NonTerminalLabel),
            "g" => Ok(Self::GrammarLabel),
            _ => Err(str),
        }
    }

    pub fn new_valued(str: String) -> (Self, String) {
        (Self::Id, str)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol {
    pub name: String,
    pub is_terminal: bool,
}

impl Parseable for Symbol {
    fn parse(stream: &mut TokenStream) -> Self {
        let token = stream.next();
        if token.token_type != GeneratorTokenType::Id {
            panic!("Expected id");
        }

        // Extract the name
        let name = token.value.clone().unwrap();

        // If the name is empty, it's an epsilon
        if name == "e" {
            return Self {
                name: "e".to_string(),
                is_terminal: true,
            };
        }

        // Check if all characters in name are uppercase or underscore; if so, it's a terminal
        let is_terminal = name.chars().all(|c| c.is_uppercase() || c == '_');

        Self { name, is_terminal }
    }
}

#[derive(Debug, Clone)]
pub struct State {
    pub num: usize,
    pub productions: Vec<Production>,
    pub transitions: HashMap<Symbol, usize>,
}

impl State {
    pub fn new(num: usize, init_prod: Production) -> Self {
        Self {
            num,
            productions: vec![init_prod],
            transitions: HashMap::new(),
        }
    }

    pub fn add_production(&mut self, prod: Production) {
        self.productions.push(prod);
    }

    pub fn add_productions(&mut self, prods: Vec<Production>) {
        self.productions.extend(prods);
    }

    pub fn add_transition(&mut self, sym: &Symbol, state: usize) {
        self.transitions.insert(sym.clone(), state);
    }
}
