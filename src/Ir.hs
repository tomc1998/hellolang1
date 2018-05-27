module Ir (astToIr) where

import Control.Monad.State.Lazy (State, get, put)
import Data.Foldable
import Ast hiding (Variable, Add)
import qualified Ast
import SymbolTable

data OpCode = Store | Load | Mul | Add | Sub | Div | Cmp
  | Je | Jne | Jg | Jl | Jge | Jle deriving (Show)
data Operand = Variable Symbol | Immf Float | Immi Int | Acc
  | Temp Int | EmptyOperand deriving (Show)

-- |Data to represent an IR instruction
data IrInstr = IrInstr OpCode Operand Operand | Label Int deriving (Show)

-- |Generate an IR label
genLabel :: State Int (IrInstr, Int)
genLabel = do
  labelNum <- get
  put $ labelNum + 1
  return (Label labelNum, labelNum)

-- |Gen instruction to store acc in temp var
stoTmp :: Int -> IrInstr
stoTmp x = IrInstr Store (Temp x) Acc

-- |Get an operator which is the reverse of the given one.
invertOp :: CmpOperator -> CmpOperator
invertOp op
  | op == Eq = Neq
  | op == Neq = Eq
  | op == Gt = Le
  | op == Ge = Lt
  | op == Lt = Ge
  | op == Le = Gt

-- |Convert a comparison operator to a jump instruction. Pass a label to jump
-- to, and a bool which should be set to 'true' to 'invert' the generation (i.e.
-- generate jne if the op is eq, le if the op is gt)
cmpOpToIrInstr :: CmpOperator -> Int -> Bool -> IrInstr
cmpOpToIrInstr op label invert
  | realOp == Eq = IrInstr Je (Immi label) EmptyOperand
  | realOp == Neq = IrInstr Jne (Immi label) EmptyOperand
  | realOp == Gt = IrInstr Jg (Immi label) EmptyOperand
  | realOp == Ge = IrInstr Jge (Immi label) EmptyOperand
  | realOp == Lt = IrInstr Jl (Immi label) EmptyOperand
  | realOp == Le = IrInstr Jle (Immi label) EmptyOperand
  where realOp = if invert then invertOp op else op

-- |Convert an AST to an intermediate representation
astToIr :: AstNode -> State Int [IrInstr]

astToIr (AstNode Program children) = do
  childrenIr <- mapM astToIr children
  return $ foldl (++) [] childrenIr

astToIr (AstNode Assignment [AstNode (Ast.Variable var) [], rhs]) = do
  rhsIr <- astToIr rhs

  return $ foldl (flip (:)) [IrInstr Store (Variable var) Acc] rhsIr

astToIr (AstNode (Operator op) [lhs, rhs]) = do
  rhsIr <- (astToIr rhs)
  lhsIr <- (astToIr lhs)
  return $ ((IrInstr Load (Temp 0) EmptyOperand):(IrInstr Add (Temp 0) Acc):lhsIr) ++ (stoTmp 0):rhsIr

astToIr (AstNode (Literal val) []) = return [IrInstr Load (Immf val) EmptyOperand]
astToIr (AstNode (Ast.Variable var) []) = return [IrInstr Load (Variable var) EmptyOperand]

astToIr (AstNode (CmpOperator op) [lhs, rhs]) = do
  rhs <- (astToIr rhs)
  lhs <- (astToIr lhs)
  lhsSto <- return $ (stoTmp 0):lhs
  -- Calc and store the lhs, then calc the rhs, then compare the lhs to the rhs.
  -- After this, the comparison flags will be set.
  return $ (rhs ++ lhsSto) ++ [IrInstr Cmp (Temp 0) Acc]

astToIr (AstNode If [(AstNode (CmpOperator op) expr), thenBody]) = do
  -- Generate a 'false' label for jumping to
  (endLabelInstr, endLabel) <- genLabel
  -- Generate code for expr & body
  exprIr <- astToIr $ AstNode (CmpOperator op) expr
  bodyIr <- astToIr thenBody
  -- Assemble
  return $ exprIr ++ [cmpOpToIrInstr op endLabel True] ++ bodyIr ++ [endLabelInstr]

astToIr (AstNode While [(AstNode (CmpOperator op) expr), body]) = do
  -- Gen 2 labels, a start & end
  (startLabelInstr, startLabel) <- genLabel
  (endLabelInstr, endLabel) <- genLabel
  -- Gen code for expr & body
  exprIr <- astToIr $ AstNode (CmpOperator op) expr
  bodyIr <- astToIr body
  -- Assemble
  return $ (startLabelInstr:exprIr) ++ [cmpOpToIrInstr op endLabel True]
    ++ bodyIr ++ [endLabelInstr]
