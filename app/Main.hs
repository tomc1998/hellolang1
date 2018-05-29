module Main where

import Lib

import System.Environment (getArgs)
import System.IO (readFile)

import Control.Monad.State.Lazy
import Control.Monad.Reader (runReader)

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
  (ir, table) <- return $ runState (Ir.astToIr ast) SymbolTable.newSymbolTable
  mapM_ (putStrLn . show) ir
  asm <- return $ runReader (SynthesisGas.synthesise ir) table
  putStrLn "\nGENERATED ASM:\n"
  putStrLn asm
