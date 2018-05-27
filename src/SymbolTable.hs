module SymbolTable (Symbol) where

import Data.Map.Lazy as Map

type Symbol = String

data SymbolMeta = None

type SymbolTable = Map.Map String SymbolMeta


