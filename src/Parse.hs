module Parse (parse) where

import qualified Token
data NTermType =
  Program
  | Stmt
  | Assignment
  | Expression
  | Term
  | Literal
  | Digit
  | FunctionCall
  | ParameterList
  | Parameter
  | Control
  | ControlKeyword
  | BooleanExpression
  | BooleanTerm
  | BooleanOperator
  | ComparisonOperator
  deriving (Show)

-- |A parse tree node
data PNode = NTerm NTermType [PNode] | Terminal String
  deriving (Show)

fromToken :: Token.Token -> PNode
fromToken t = Terminal $ Token.tokVal t

type Parser = [Token.Token] -> (PNode, [Token.Token])

parseTerm :: Parser
parseTerm (t0:t1:ts)
  | Token.tokType t0 /= Token.Identifier &&
    Token.tokType t0 /= Token.Literal
  = error $ "Expected identifier or literal, got " ++ (show t0)
  | (op /= "*" && op /= "/") = (NTerm Term [fromToken t0], t1:ts)
  | otherwise = (NTerm Term [fromToken t0, Terminal op, term], rest)
  where
    op = Token.tokVal t1
    (term, rest) = parseTerm ts

parseExpression :: Parser
parseExpression ts
  | null rest || (op /= "+" && op /= "-") = (NTerm Expression [term], rest)
  | otherwise = (NTerm Expression [term, Terminal op, expr], restExpr)
  where
    (term, rest) = parseTerm ts
    op = Token.tokVal $ head rest
    (expr, restExpr) = parseExpression $ tail rest

parseParameterList :: Parser
parseParameterList ts
  | null rest = error "Unexpected EOF in function call parameter list"
  | Token.tokVal (head rest) == "," =
    (NTerm Expression [expr, fromToken $ head rest, parameters],
     parametersRest)
  | Token.tokVal (head rest) == ")" = (NTerm Expression [expr], rest)
  | otherwise = error $ "Expected ',' or ')', got " ++ (show $ head rest)
  where
    (expr, rest) = parseExpression ts
    (parameters, parametersRest) = parseParameterList $ tail rest

parseFunctionCall :: Parser
parseFunctionCall (t0:t1:ts)
  | null rest = error "Unexpected end of file in function call parameter list"
  | (Token.tokVal $ head rest) /= ")"
  = error $ "Error, expected ')', got " ++ (show $ head rest)
  | otherwise = (NTerm FunctionCall
                 [fromToken t0
                 ,fromToken t1
                 ,parameters
                 ,fromToken $ head rest], tail rest)
  where (parameters, rest) = parseParameterList ts
parseFunctionCall _ = error "Unexpected EOF"

parseAssignment :: Parser
parseAssignment (t0:t1:ts) =
  (NTerm Assignment [fromToken t0, fromToken t1, expr], rest)
  where (expr, rest) = parseExpression ts
parseAssignment _ = error "Unexpected EOF"

parseBooleanTerm :: Parser
parseBooleanTerm (t:ts)
  | Token.tokVal t == "!" =
    (NTerm BooleanTerm [fromToken t, unaryOpTerm], unaryOpTermRest)
  | null expr0Rest = error "Unexpected EOF, expected comparison operator"
  | not $ elem (Token.tokVal $ head expr0Rest) ["==", ">", "<", ">=","<="] =
    error $ "Error, expected comparison operator, found "
              ++ (show $ head expr0Rest)
  | otherwise = (NTerm BooleanTerm [expr0, fromToken $ head expr0Rest, expr1],
                 expr1Rest)
  where
    (expr0, expr0Rest) = parseExpression (t:ts)
    (expr1, expr1Rest) = parseExpression $ tail expr0Rest
    -- This is used when the head token is the ! operator
    (unaryOpTerm, unaryOpTermRest) = parseTerm ts

parseBooleanExpression :: Parser
parseBooleanExpression ts
  | null termRest = (NTerm BooleanExpression [term], termRest)
  | elem (Token.tokVal $ head termRest) ["&&", "||"] =
    (NTerm BooleanExpression [term, fromToken $ head termRest, expr], exprRest)
  | otherwise =
    (NTerm BooleanExpression [term], termRest)
  where
    (term, termRest) = parseBooleanTerm ts
    (expr, exprRest) = parseBooleanExpression $ tail termRest

parseControl :: Parser
parseControl (t0:t1:ts)
  | Token.tokVal t1 /= "(" = error $ "Error, expected '(', found " ++ (show t1)
  | length rest < 4 = error "Unexpected EOF"
  | Token.tokVal (head rest) /= ")" = error $ "Error, expected ')', found "
    ++ (show $ head rest)
  | Token.tokVal (rest !! 1) /= "{" = error $ "Error, expected '{', found "
    ++ (show $ rest !! 1)
  | Token.tokVal (head programRest) /= "}"
  = error $ "Error, expected '}', found " ++ (show $ head programRest)
  | otherwise = (NTerm Control [fromToken t0
                               ,fromToken t1
                               ,booleanExpr
                               ,fromToken $ head rest
                               ,fromToken $ rest !! 1
                               ,program
                               ,fromToken $ (head programRest)], tail programRest)
  where
    (booleanExpr, rest) = parseBooleanExpression ts
    (program, programRest) = parseProgram (tail $ tail rest)

parseStmt :: Parser
parseStmt (t0:t1:ts) = nextParseFunc (t0:t1:ts)
  where
    nextParseFunc :: Parser
    nextParseFunc =
      if Token.tokType t0 == Token.Keyword then parseControl
      else if Token.tokType t0 == Token.Identifier then
        if Token.tokVal t1 == "("
        then parseFunctionCall
        else if Token.tokVal t1 == "=" then
          parseAssignment
          else error $ "Unexpected token " ++ (show (t0:t1:ts))
               ++ ", expected '=' or '('"
        else error $ "Unexpected token " ++ (show (t0:t1:ts))
parseStmt _ = error "Unexpected EOF"

parseProgram :: Parser
parseProgram [] = (NTerm Program [], [])
parseProgram (t:ts)
  | length rest >= 2 && Token.tokVal (head rest) == ";" && Token.tokVal (rest !! 1) == "}" =
    (NTerm Program [stmt, Terminal ";"], tail rest)
  | Token.tokVal (head rest) == ";" =
    (NTerm Program [stmt, fromToken $ head rest, program], restProgram)
  | otherwise = error $ "Expected ';', found " ++ (show $ head rest)
  where
    (stmt, rest) = parseStmt $ t:ts
    (program, restProgram) = parseProgram $ tail rest

parse :: [Token.Token] -> PNode
parse ts = fst $ parseProgram ts

