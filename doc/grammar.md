# Hellolang grammar

Program ::= Stmt ";" Program
Program ::= Stmt

Stmt ::= Assignment | FunctionCall | Control

Assignment ::= Identifier "=" Expression
Expression ::= Term "+" Expression | Term "-" Expression | Term
Term ::= Identifier | Literal | Identifier "*" Term | Literal "*" Term | Identifier "/" Term | Literal "/" Term
Literal ::= Digit Literal | Digit
Digit ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

FunctionCall ::= Identifier "(" ParameterList ")"
ParameterList ::= Parameter "," ParameterList
ParameterList ::= Parameter
Parameter ::= Expression

Control ::= ControlKeyword "(" BooleanExpression ") "{" Program "}"
ControlKeyword ::= "if" | "while"
BooleanExpression ::= BooleanTerm BooleanOperator BooleanExpression
BooleanTerm ::= BooleanOperator BooleanTerm
BooleanTerm ::= Expression ComparisonOperator Expression
BooleanOperator ::= "&&" | "||" | "!"
ComparisonOperator ::= "==" | ">" | "<" | ">=" |"<="
