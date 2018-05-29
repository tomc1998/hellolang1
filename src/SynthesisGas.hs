module SynthesisGas (synthesise) where

import Ir
import SymbolTable (SymbolTable)
import Control.Monad.Reader (Reader, ask)

toGasInstr :: IrInstr -> Reader SymbolTable String

toGasInstr (Label name) = return $ name ++ ":\n"

toGasInstr (IrInstr Call (JumpLabel name) _) = return $ "call " ++ name ++ "\n"

toGasInstr i = return ""

synthesise :: [IrInstr] -> Reader SymbolTable String
synthesise ir = do
  instructions <- mapM toGasInstr ir
  return $ foldl (++) [] instructions
