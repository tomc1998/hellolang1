module Main where

import Lib

import System.Environment (getArgs)
import System.IO (readFile)

import Control.Monad.State.Lazy

import qualified Lex
import qualified Parse
import qualified Ast
import qualified Ir
import qualified SymbolTable

main :: IO ()
main = do
  args <- getArgs
  files <- mapM readFile args
  tokens <- return $ foldr (++) [] $ map Lex.lex files
  parseTree <- return $ Parse.parse tokens
  ast <- return $ Ast.parseTreeToAst parseTree
  ir <- return $ fst $ runState (Ir.astToIr ast) SymbolTable.newSymbolTable
  print ast
  mapM_ print ir
