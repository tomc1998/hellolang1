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
import qualified SynthesisGas

main :: IO ()
main = do
  args <- getArgs
  files <- mapM readFile args
  tokens <- return $ foldr (++) [] $ map Lex.lex files
  parseTree <- return $ Parse.parse tokens
  ast <- return $ Ast.parseTreeToAst parseTree
  (ir, table) <- return $ runState (Ir.generateIr ast) SymbolTable.newSymbolTable
  dataSection <- return $ fst $ runState Ir.initImmediate table
  asm <- return $ fst $ runState (SynthesisGas.synthesise dataSection ir) table
  putStrLn "Writing to out.s"
  writeFile "out.s" asm
