module Lex (Lex.lex) where

import qualified Data.Char
import qualified Token
import Data.List

consumeNumber :: String -> (String, String)
consumeNumber (i:is)
  | Data.Char.isDigit i =
    let (number, rest) = consumeNumber is in
    (i:number, rest)
  | otherwise = ("", i:is)

lex :: String -> [Token.Token]
lex (i:is)
  | Data.Char.isSpace i = Lex.lex is
  | i == ';'
    || i == '('
    || i == ')'
    || i == '{'
    || i == '}'
  = (Token.Token [i] Token.Punc) : Lex.lex is
  | Data.Char.isDigit i =
    let (number, rest) = consumeNumber is in
    (Token.Token (i:number) Token.Literal) : Lex.lex rest
  | i == '+'
    || i == '-'
    || i == '/'
    || i == '*'
  = (Token.Token [i] Token.Operator) : Lex.lex is
  | i == '>' || i == '<' || i == '=' =
    if null is
      then [Token.Token [i] Token.Operator]
      else if head is == '='
           then (Token.Token [i, head is] Token.Operator) : Lex.lex (tail is)
           else (Token.Token [i] Token.Operator) : Lex.lex is
  | untilSpace == "if" || untilSpace == "while"
  = (Token.Token untilSpace Token.Keyword) : Lex.lex restAfterSpace
  | otherwise
  = (Token.Token untilSpace Token.Identifier) : Lex.lex restAfterSpace
  where
    untilSpace :: String
    untilSpace = takeWhile (not . Data.Char.isSpace) (i:is)
    restAfterSpace :: String
    restAfterSpace = dropWhile (not . Data.Char.isSpace) (i:is)


