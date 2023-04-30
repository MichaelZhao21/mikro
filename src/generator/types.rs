use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
};

use crate::lexer::Loc;

pub struct TokenStream {
    pub tokens: Vec<GeneratorToken>,
    pub index: usize,
}

impl TokenStream {
    pub fn new(tokens: Vec<GeneratorToken>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn peek(&self) -> &GeneratorToken {
        self.tokens.get(self.index).unwrap()
    }

    pub fn next(&mut self) -> &GeneratorToken {
        let token = self.tokens.get(self.index).unwrap();
        self.index += 1;
        token
    }
}

pub trait Parseable {
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
            grammar_section.append(&mut Production::parse_list(stream));
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

    pub fn next_sym(&self) -> Option<Symbol> {
        match self.rhs.get(self.pos) {
            Some(sym) => Some(sym.clone()),
            None => None,
        }
    }

    pub fn is_complete(&self) -> bool {
        self.pos == self.rhs.len()
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    pub fn parse_list(stream: &mut TokenStream) -> Vec<Self> {
        let mut productions = Vec::<Production>::new();

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

        // Get the list of rhs's, split by pipes
        while stream.peek().token_type != GeneratorTokenType::Semi
            && stream.peek().token_type != GeneratorTokenType::EOF
        {
            // List of symbols on the rhs
            let mut rhs = Vec::<Symbol>::new();

            // Loop through the rhs until we hit a pipe or semicolon
            while stream.peek().token_type != GeneratorTokenType::Pipe
                && stream.peek().token_type != GeneratorTokenType::Semi
                && stream.peek().token_type != GeneratorTokenType::EOF
            {
                // If the symbol is an epsilon, don't add it and continue
                if stream.peek().token_type == GeneratorTokenType::Epsilon {
                    stream.next();
                    break;
                } else {
                    rhs.push(Symbol::parse(stream));
                }
            }

            // Add the production to the list
            productions.push(Production::new(lhs.clone(), rhs));

            // Check for pipe
            if stream.peek().token_type == GeneratorTokenType::Pipe {
                stream.next();
            }
        }

        // Consume the semicolon
        stream.next();

        productions
    }
}

impl Parseable for Production {
    fn parse(_: &mut TokenStream) -> Self {
        panic!("Use parse_list instead");
    }
}

impl Display for Production {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> ", self.lhs)?;
        for (i, sym) in self.rhs.iter().enumerate() {
            if i == self.pos {
                write!(f, ". ")?;
            }
            write!(f, "{} ", sym)?;
        }
        if self.pos == self.rhs.len() {
            write!(f, ".")?;
        }
        Ok(())
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
    Epsilon,
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
            "e" => Ok(Self::Epsilon),
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

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Symbol {
    pub fn new(name: String, is_terminal: bool) -> Self {
        Self { name, is_terminal }
    }

    /// Returns an empty symbol, which is used for None types
    pub fn none() -> Self {
        Self {
            name: "".to_string(),
            is_terminal: true,
        }
    }
}

#[derive(Debug, Clone)]
pub struct State {
    pub num: usize,
    pub productions: Vec<Production>,
    pub transitions: HashMap<Symbol, usize>,
}

impl State {
    pub fn new(num: usize, init_prod: Production, grammar: &Grammar) -> Self {
        let mut new_state = Self {
            num,
            productions: vec![init_prod],
            transitions: HashMap::new(),
        };
        new_state.closure(grammar);
        new_state
    }

    pub fn add_production(&mut self, prod: Production) {
        self.productions.push(prod);
    }

    pub fn add_productions(&mut self, prods: Vec<Production>) {
        self.productions.extend(prods);
    }

    pub fn add_transition(&mut self, sym: Symbol, state: usize) {
        self.transitions.insert(sym, state);
    }

    pub fn closure(&mut self, grammar: &Grammar) {
        // Loop until nothing changes
        let mut changed = true;
        while changed {
            changed = false;

            // Vector of new productions to add
            let mut new_prods = Vec::<Production>::new();

            // Loop over the productions in the state
            for prod in &self.productions {
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
                    if self.productions.contains(&g_prod) {
                        continue;
                    }

                    // Add the production to the state
                    new_prods.push(g_prod.clone());

                    // Set changed flag
                    changed = true;
                }
            }

            // Append the new productions to the state
            self.add_productions(new_prods.clone());
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "[State {}]", self.num)?;
        for prod in &self.productions {
            writeln!(f, "\t{}", prod)?;
        }
        writeln!(f, "\tTransitions:")?;
        for (sym, state) in &self.transitions {
            writeln!(f, "\t\t{} -> {}", sym.name, state)?;
        }
        Ok(())
    }
}
