module SymbolTable (Symbol
                   ,SymbolMeta(Function, Label, None)
                   ,SymbolTable(SymbolTable)
                   ,newSymbolTable
                   ,putSym
                   ,memberSym
                   ,getSym
                   ,genSym) where

import Data.Map.Lazy as Map
import Control.Monad.State.Lazy (State, get, put)

type Symbol = String

data SymbolMeta = Function { fNumParams :: Int } | Label | None

data SymbolTable = SymbolTable (Map.Map String SymbolMeta) Int

newSymbolTable :: SymbolTable
newSymbolTable = SymbolTable Map.empty 0

-- |Add a symbol to the table
putSym :: String -> SymbolMeta -> State SymbolTable ()
putSym s d = do
  (SymbolTable table n) <- get
  put $ SymbolTable (Map.insert s d table) n
  return ()

-- |Returns true if the given string is in the symbol table
memberSym :: String -> State SymbolTable Bool
memberSym s = do
  SymbolTable table _ <- get
  return $ Map.member s table

-- |Gets a symbol from the table, or Nothing if it doesn't exist
getSym :: String -> State SymbolTable (Maybe SymbolMeta)
getSym s = do
  SymbolTable table _ <- get
  return $ Map.lookup s table

-- |Generate a symbol. DOESN'T add it to the symbol table!
genSym :: State SymbolTable String
genSym = do
  -- First, get and increment the value in the symbol table
  (SymbolTable table currGenVal) <- get
  sym <- return $ "gen_" ++ (show $ currGenVal + 1)
  put $ SymbolTable table (currGenVal + 1)
  isMember <- memberSym sym
  if isMember then genSym else return sym
