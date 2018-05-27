module Ir (astToIr) where

import Control.Monad.State.Lazy (State)
import Ast hiding (Variable, Add)
import qualified Ast
import SymbolTable

data OpCode = Store | Load | Mul | Add | Sub | Div | Cmp deriving (Show)
data Operand = Variable Symbol | Immediate Float | Acc | Temp Int | EmptyOperand deriving (Show)

-- |Data to represent an IR instruction
data IrInstr = IrInstr OpCode Operand Operand deriving (Show)

-- |Gen instruction to store acc in temp var
stoTmp :: Int -> IrInstr
stoTmp x = IrInstr Store (Temp x) Acc

-- |Convert an AST to an intermediate representation
astToIr :: AstNode -> [IrInstr]

astToIr (AstNode Program children) =
  foldl (++) [] $ map astToIr children

astToIr (AstNode Assignment [AstNode (Ast.Variable var) [], rhs]) =
  foldl (flip (:)) [IrInstr Store (Variable var) Acc] $ astToIr rhs

astToIr (AstNode (Operator op) [lhs, rhs]) =
  -- Calc LHS, perform add into tmp 0, then load into acc
  ((IrInstr Load (Temp 0) EmptyOperand):(IrInstr Add (Temp 0) Acc):(astToIr lhs))
  -- Calc RHS and store in tmp
  ++ (stoTmp 0):(astToIr rhs)

astToIr (AstNode (Literal val) []) = [IrInstr Load (Immediate val) EmptyOperand]
astToIr (AstNode (Ast.Variable var) []) = [IrInstr Load (Variable var) EmptyOperand]
