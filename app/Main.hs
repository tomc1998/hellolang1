module Main where

import Lib

import System.Environment (getArgs)
import System.IO (readFile)

import qualified Lex
import qualified Parse

main :: IO ()
main = do
  args <- getArgs
  files <- mapM readFile args
  tokens <- return $ foldr (++) [] $ map Lex.lex files
  parseTree <- return $ Parse.parse tokens
  print parseTree
