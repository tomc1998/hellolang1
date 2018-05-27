module Main where

import Lib

import System.Environment (getArgs)
import System.IO (readFile)

import qualified Lex

main :: IO ()
main = do
  args <- getArgs
  files <- mapM readFile args
  tokens <- return $ foldr (++) [] $ map Lex.lex files
  mapM_ print tokens
