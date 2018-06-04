module Ast (parseTreeToAst, AstNode(AstNode)
           ,AstNodeData(Assignment, FunctionCall, If, While, Operator,
                         CmpOperator, Literal, Variable, Program)
           ,Operator(Add, Sub, Mul, Div)
           ,CmpOperator(Neq, Eq, Gt, Ge, Lt, Le)
           ,BoolOperator(LAnd, LOr)) where

import Debug.Trace (trace)
import Parse hiding (Assignment, FunctionCall, Literal, Program)
import qualified Parse (NTermType(Assignment, FunctionCall, Literal, Program))

data Operator = Add | Sub | Mul | Div deriving (Show, Eq)
data CmpOperator = Neq | Eq | Gt | Ge | Lt | Le deriving (Show, Eq)
data BoolOperator = LAnd | LOr deriving (Show, Eq)

data AstNodeData = Assignment
  | FunctionCall String
  | If
  | While
  | Operator Operator
  | CmpOperator CmpOperator
  | BoolOperator BoolOperator
  | Literal Float
  | Variable String
  | Program
  deriving (Show)

data AstNode = AstNode AstNodeData [AstNode] deriving Show

parseOperator :: String -> Operator
parseOperator "+" = Add
parseOperator "-" = Sub
parseOperator "*" = Mul
parseOperator "/" = Div

parseCmpOperator :: String -> CmpOperator
parseCmpOperator x | trace x False = undefined
parseCmpOperator "==" = Eq
parseCmpOperator ">" = Gt
parseCmpOperator ">=" = Ge
parseCmpOperator "<" = Lt
parseCmpOperator "<=" = Le

parseBoolOperator :: String -> BoolOperator
parseBoolOperator "&&" = LAnd
parseBoolOperator "||" = LOr


parseTreeToAst :: PNode -> AstNode

parseTreeToAst (NTerm Parse.Program statements) =
  AstNode Program $ map parseTreeToAst filtered
  where filtered = filter (/= (Terminal ";")) statements

parseTreeToAst (NTerm Parse.Stmt [child]) =
  parseTreeToAst child

parseTreeToAst (NTerm Parse.Assignment children) =
  AstNode Assignment [
      parseTreeToAst $ children !! 0,
      parseTreeToAst $ children !! 2
  ]

parseTreeToAst (NTerm Identifier [Terminal term]) = AstNode (Variable term) []

parseTreeToAst (Terminal term) = AstNode (Literal (read term :: Float)) []

parseTreeToAst (NTerm Control [Terminal "while", _, expr, _, _, thenProg, _]) =
  AstNode While [parseTreeToAst expr, parseTreeToAst thenProg]
parseTreeToAst (NTerm Control [Terminal "if", _, expr, _, _, thenProg, _]) =
  AstNode If [parseTreeToAst expr, parseTreeToAst thenProg]

parseTreeToAst (NTerm Parse.FunctionCall [Terminal functionName, _, NTerm _ parameters, _]) =
  AstNode (FunctionCall functionName) $ map parseTreeToAst parameters


parseTreeToAst (NTerm Expression [term]) = parseTreeToAst term
parseTreeToAst (NTerm Expression [term, Terminal op, expr]) =
  AstNode (Operator $ parseOperator op) [parseTreeToAst term, parseTreeToAst expr]
parseTreeToAst (NTerm BooleanExpression [term]) = parseTreeToAst term
parseTreeToAst (NTerm BooleanExpression [term, Terminal op, expr]) =
  AstNode (BoolOperator $ parseBoolOperator op) [parseTreeToAst term, parseTreeToAst expr]

parseTreeToAst (NTerm Term [v]) = parseTreeToAst v
parseTreeToAst (NTerm Term [v, Terminal op, term]) =
  AstNode (Operator $ parseOperator op) [parseTreeToAst v, parseTreeToAst term]

parseTreeToAst (NTerm BooleanTerm [v]) = parseTreeToAst v
parseTreeToAst (NTerm BooleanTerm [expr0, Terminal op, expr1]) =
  AstNode (CmpOperator $ parseCmpOperator op) [parseTreeToAst expr0, parseTreeToAst expr1]
