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
lex [] = []
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
  | untilNonIdent == "if" || untilNonIdent == "while"
  = (Token.Token untilNonIdent Token.Keyword) : Lex.lex restAfterNonIdent
  | otherwise
  = (Token.Token untilNonIdent Token.Identifier) : Lex.lex restAfterNonIdent
  where
    restAfterNonIdent :: String
    restAfterNonIdent = dropWhile Data.Char.isAlpha (i:is)
    untilNonIdent :: String
    untilNonIdent = takeWhile Data.Char.isAlpha (i:is)
