# Mikro

Language that I'm making for fun :D The name of this language is a word play on Mikey hehe like "micro".

## BNF Grammar

> Note: This is WIP and will grow as I add more features to the language!

```
Program                     ➞  FunctionDeclerations
FunctionDeclerations        ➞  FunctionDecleration FunctionDeclerations
FunctionDecleration         ➞  func id lParen ParameterList rParen colon Type lCurly StatementList rCurly
StatementList               ➞  Statement StatementList | λ
Statement                   ➞  DeclerationStatement | AssignmentStatement | PrintStatement
DeclerationStatement        ➞  let id colon Type assign Expression semi
AssignmentStatement         ➞  id assign Expression semi
PrintStatement              ➞  aiya Expression semi
Expression                  ➞  CallExpression | LiteralExpression | BinaryExpression
CallExpression              ➞  id lParen Expression rParen
LiteralExpression           ➞  IntLiteral
BinaryExpression            ➞  Expression BinaryOp Expression
BinaryOp                    ➞  + | - | * | /
IntLiteral                  ➞  {[0-9]+}
Type                        ➞  int | string | float | bool
```

### Lexing Rules

- **String Literals** start and end with a double quotation mark
- **Integer Literals** should only contain digits [0-9]
- **Identifiers** should start with an alpha character and can contain alphanumeric characters. They should also not be any keywords

# Time to be ridiculous

I have decided to make my own parser generator :clown_face:!

## BNF Grammar for Grammar File

```
Grammar             ➞  SectionList
SectionList         ➞  TermSection NonTermSection GrammarSection
TermSection         ➞  divider terminalLabel TermList divider
NonTermSection      ➞  divider nonTerminalLabel TermList divider
TermList            ➞  id TermList | λ
GrammarSection      ➞  divider grammarLabel ProductionList divider
ProductionList      ➞  Production ProductionList | λ
Production          ➞  id arrow RhsList semi
RhsList             ➞  id pipe RhsListTail | λ
RhsList             ➞  id | id pipe RhsListTail
```

### Terminals and Nonterminals

`id` values are the terminals and non-terminals defined in the grammar. Terminals should be all capital letters and non-terminals should be camel case (the parser generator will panic if this is not true). If a symbol is used but not defined in the grammar section, an error will be thrown.

### Generated Code

Terminals

```rust
trait Symbol {
    name: String,
    is_terminal: bool,
}

struct Production {
    lhs: Token,
    rhs: Vec<Symbol>
    dot: usize
}

struct State {
    num: usize,
    productions: Vec<Production>,
    transitions: HashMap<Symbol, usize>
}
```