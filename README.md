# Mikro

Language that I'm making for fun :D The name of this language is a word play on Mikey hehe like "micro".

## BNF Grammar

> Note: This is WIP and will grow as I add more features to the language!

```
Program                     ➞  FunctionDeclerations
FunctionDeclerations        ➞  FunctionDecleration FunctionDeclerations
FunctionDecleration         ➞  func id ( ParameterList ) : Type { StatementList }
StatementList               ➞  Statement StatementList | λ
Statement                   ➞  DeclerationStatement | AssignmentStatement | PrintStatement
DeclerationStatement        ➞  let id: Type = Expression ;
AssignmentStatement         ➞  id = Expression ;
PrintStatement              ➞  aiya Expression ;
Expression                  ➞  CallExpression | LiteralExpression | BinaryExpression
CallExpression              ➞  id ( Expression )
LiteralExpression           ➞  IntLiteral
BinaryExpression            ➞  Expression BinaryOp Expression
BinaryOp                    ➞  + | - | * | /
IntLiteral                  ➞  {[0-9]+}
```
