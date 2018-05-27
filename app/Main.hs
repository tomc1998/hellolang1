module Main where

import Lib

import System.Environment (getArgs)
import System.IO (readFile)

import qualified Lex
import qualified Parse
import qualified Ast

main :: IO ()
main = do
  args <- getArgs
  files <- mapM readFile args
  tokens <- return $ foldr (++) [] $ map Lex.lex files
  parseTree <- return $ Parse.parse tokens
  ast <- return $ Ast.parseTreeToAst parseTree
  print ast
