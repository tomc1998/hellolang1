module Ir (generateIr
          ,initImmediate
          ,IrInstr(IrInstr, Label)
          ,Operand(Variable, Immf, Acc, Param, EmptyOperand, JumpLabel, StackTop, Deref
                  ,Sp, Offset)
          ,OpCode(Section, Data, Push, Pop, Store, Load, Mul, Add, Sub, Div,
                  Cmp, Jmp, Je, Jne, Jg, Jl, Jge, Jle, Call)
          ) where

import Control.Monad.State.Lazy (State, get)
import Data.Foldable
import Ast hiding (Variable, Add, Sub, Div, Mul)
import qualified Ast
import SymbolTable hiding (Label, Variable)
import qualified SymbolTable

data OpCode = Section String | Data Int | Store | Load | Mul | Add | Sub | Div | Cmp
  | Push | Pop | Jmp| Je | Jne | Jg | Jl | Jge | Jle | Call deriving (Show)
data Operand = Offset Int Operand | Deref Operand | Sp | Variable Symbol
  | Immf Float | Acc | StackTop | Param Int | EmptyOperand
  | JumpLabel Symbol deriving (Show)

-- |Data to represent an IR instruction
data IrInstr = IrInstr OpCode Operand Operand | Label Symbol deriving (Show)

-- |Gen instruction to store acc in param var
stoParam :: Int -> IrInstr
stoParam x = IrInstr Store (Param x) EmptyOperand

pushOp :: IrInstr
pushOp = IrInstr Push Acc EmptyOperand

popOp :: IrInstr
popOp = IrInstr Pop EmptyOperand EmptyOperand

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
cmpOpToIrInstr :: CmpOperator -> Symbol -> Bool -> IrInstr
cmpOpToIrInstr op label invert
  | realOp == Eq = IrInstr Je (JumpLabel label) EmptyOperand
  | realOp == Neq = IrInstr Jne (JumpLabel label) EmptyOperand
  | realOp == Gt = IrInstr Jg (JumpLabel label) EmptyOperand
  | realOp == Ge = IrInstr Jge (JumpLabel label) EmptyOperand
  | realOp == Lt = IrInstr Jl (JumpLabel label) EmptyOperand
  | realOp == Le = IrInstr Jle (JumpLabel label) EmptyOperand
  where realOp = if invert then invertOp op else op

astOpAsOpCode :: Operator -> OpCode
astOpAsOpCode Ast.Add = Add
astOpAsOpCode Ast.Sub = Sub
astOpAsOpCode Ast.Mul = Mul
astOpAsOpCode Ast.Div = Div

generateIr :: AstNode -> State SymbolTable [IrInstr]
generateIr n = astToIr n

-- |Convert an AST to an intermediate representation
astToIr :: AstNode -> State SymbolTable [IrInstr]

astToIr (AstNode Program children) = do
  childrenIr <- mapM astToIr children
  return $ foldl (++) [] childrenIr

astToIr (AstNode Assignment [AstNode (Ast.Variable var) [], rhs]) = do
  putVar var
  rhsIr <- astToIr rhs
  return $ foldl (flip (:)) [IrInstr Store (Variable var) EmptyOperand] rhsIr

astToIr (AstNode (Operator op) [lhs, rhs]) = do
  rhsIr <- (astToIr rhs)
  lhsIr <- (astToIr lhs)
  return $ (popOp:(IrInstr (astOpAsOpCode op) StackTop EmptyOperand):lhsIr) ++ pushOp:rhsIr

astToIr (AstNode (Literal val) []) = do
  sym <- putHardf val
  return [IrInstr Load (Deref $ JumpLabel sym) EmptyOperand]
astToIr (AstNode (Ast.Variable var) []) = do
  putVar var
  return [IrInstr Load (Variable var) EmptyOperand]

astToIr (AstNode (CmpOperator op) [lhs, rhs]) = do
  rhs <- (astToIr rhs)
  lhs <- (astToIr lhs)
  rhsSto <- return $ rhs ++ [pushOp]
  -- Calc and store the lhs, then calc the rhs, then compare the lhs to the rhs.
  -- After this, the comparison flags will be set.
  return $ rhsSto ++ lhs ++ [popOp, IrInstr Cmp (Deref Sp) EmptyOperand]

astToIr (AstNode If [(AstNode (CmpOperator op) expr), thenBody]) = do
  -- Generate a 'false' label for jumping to
  sym <- genSym
  labelInstr <- return $ Label sym
  putSym sym SymbolTable.Label
  -- Generate code for expr & body
  exprIr <- astToIr $ AstNode (CmpOperator op) expr
  bodyIr <- astToIr thenBody
  -- Assemble
  return $ exprIr ++ [cmpOpToIrInstr op sym True] ++ bodyIr ++ [labelInstr]

astToIr (AstNode While [(AstNode (CmpOperator op) expr), body]) = do
  -- Gen 2 labels, a start & end
  startSym <- genSym
  endSym <- genSym
  startInstr <- return $ Label startSym
  endInstr <- return $ [IrInstr Jmp (JumpLabel startSym) EmptyOperand, Label endSym]
  putSym startSym SymbolTable.Label
  putSym endSym SymbolTable.Label
  -- Gen code for expr & body
  exprIr <- astToIr $ AstNode (CmpOperator op) expr
  bodyIr <- astToIr body
  -- Assemble
  return $ (startInstr:exprIr) ++ [cmpOpToIrInstr op endSym True]
    ++ bodyIr ++ endInstr

astToIr (AstNode (FunctionCall name) parameters) = do
  -- evaluate all the parameters
  evaluated <- mapM astToIr parameters
  zipped <- return $ zip [0..] evaluated
  -- fold all the parameters & add them to the param variables
  folded <- return $ foldl (\list (num, param) -> list ++ param ++ [stoParam num]) [] zipped
  -- Now return the final function call
  return $ folded ++ [IrInstr Call (JumpLabel name) EmptyOperand]

-- |Given a symbol table, return instructions which initialise hardcoded values
initImmediate :: State SymbolTable [IrInstr]
initImmediate = do
  table <- SymbolTable.assocs
  onlyImmediates <- return $
    filter (\(k, v) -> case v of
               Hardf v -> True
               _ -> False) table
  mapped <- return $ map (\(k, v) ->
                   case v of
                     Hardf f -> [Label k, IrInstr (Data 4) (Immf f) EmptyOperand])
            onlyImmediates
  return $ foldl (++) [] mapped
