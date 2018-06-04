module SymbolTable (Symbol
                   ,SymbolMeta(Function, Label, Variable, Hardf, None)
                   ,SymbolTable(SymbolTable)
                   ,newSymbolTable
                   ,putSym
                   ,putVar
                   ,putHardf
                   ,memberSym
                   ,getSym
                   ,genSym
                   ,SymbolTable.assocs) where

import Data.Map.Lazy as Map
import Control.Monad.State.Lazy (State, get, put)

type Symbol = String

data SymbolMeta = Function { fNumParams :: Int } | Label | Variable Int | Hardf Float | None

data SymbolTable = SymbolTable {
  table :: (Map.Map Symbol SymbolMeta)
  ,symNum :: Int -- For generating symbols
  ,varLoc :: Int -- For generating var locations
  }

newSymbolTable :: SymbolTable
newSymbolTable = SymbolTable Map.empty 0 0

-- |Add a variable (with an auto-generated location) to the table
putVar :: Symbol -> State SymbolTable ()
putVar s = do
  (SymbolTable table n loc) <- get
  isMember <- memberSym s
  if not isMember then do
    put $ SymbolTable table n (loc + 1)
    putSym s $ Variable loc
    else return ()

-- |Generate a hardcoded symbol, associate it with the given flow, & return the
-- generated symbol
putHardf :: Float -> State SymbolTable Symbol
putHardf f = do
  sym <- genSym
  putSym sym (Hardf f)
  return sym


-- |Add a symbol to the table
putSym :: Symbol -> SymbolMeta -> State SymbolTable ()
putSym s d = do
  (SymbolTable table n0 n1) <- get
  put $ SymbolTable (Map.insert s d table) n0 n1
  return ()

-- |Returns true if the given string is in the symbol table
memberSym :: Symbol -> State SymbolTable Bool
memberSym s = do
  SymbolTable table _ _ <- get
  return $ Map.member s table

-- |Gets a symbol from the table, or Nothing if it doesn't exist
getSym :: Symbol -> State SymbolTable (Maybe SymbolMeta)
getSym s = do
  SymbolTable table _ _ <- get
  return $ Map.lookup s table

-- |Generate a symbol. DOESN'T add it to the symbol table!
genSym :: State SymbolTable Symbol
genSym = do
  -- First, get and increment the value in the symbol table
  (SymbolTable table currGenVal n) <- get
  sym <- return $ "gen_" ++ (show $ currGenVal + 1)
  put $ SymbolTable table (currGenVal + 1) n
  isMember <- memberSym sym
  if isMember then genSym else return sym

assocs :: State SymbolTable [(Symbol, SymbolMeta)]
assocs = do
  (SymbolTable table _ _) <- get
  return $ Map.assocs table
