module SynthesisGas (synthesise) where

import Ir
import SymbolTable (SymbolTable, getSym)
import qualified SymbolTable
import Control.Monad.State.Lazy (State, get)

convertOperand :: Operand -> State SymbolTable String
convertOperand (Variable x) = do
  Just (SymbolTable.Variable loc) <- getSym x
  return $ ('-':(show $ 4 * (loc+1))) ++ "(%rbp)"
convertOperand (Immf x) = return $ show x
convertOperand Acc = return "%st(0)"
convertOperand StackTop = return "4(%rsp)"
convertOperand (Param x)
  | x > 7 = error "Don't support calling functions with more than 8 parameters"
  | otherwise = return $ "%xmm" ++ (show x)
convertOperand EmptyOperand = return ""
convertOperand (JumpLabel s) = return s
convertOperand Sp = return "%rsp"
convertOperand (Offset n x) = do
  op <- convertOperand x
  return $ (show n) ++ "(" ++ op ++ ")"
convertOperand (Deref o) = do
  inner <- convertOperand o
  return $ "(" ++ inner ++ ")"

convertOpCode :: OpCode -> State SymbolTable String
convertOpCode Store = return "fstp"
convertOpCode Load = return "fld"
convertOpCode Mul = return "fmul"
convertOpCode Add = return "fadd"
convertOpCode Sub = return "fsub"
convertOpCode Div = return "fdiv"
convertOpCode Cmp = return "fcomp"
convertOpCode Push = return "push"
convertOpCode Pop = return "pop"
convertOpCode Jmp = return "jmp"
convertOpCode Je = return "je"
convertOpCode Jne = return "jne"
convertOpCode Jg = return "ja"
convertOpCode Jge = return "jae"
convertOpCode Jl = return "jb"
convertOpCode Jle = return "jbe"
convertOpCode Call = return "call"
convertOpCode (Section s) = return $ '.':s
convertOpCode (Data 4) = return ".single"
convertOpCode (Data 8) = return ".double"
convertOpCode (Data x) = error $ "Unknown data size " ++ (show x)

toGasInstr :: IrInstr -> State SymbolTable String
toGasInstr (Label name) = return $ name ++ ":\n"

toGasInstr (IrInstr Store (Param x) _) =
  return "sub $4, %rsp\n\
\fstp 4(%rsp)\n\
\cvtss2sd 4(%rsp), %xmm0\n\
\add $4, %rsp\n"

toGasInstr (IrInstr Push Acc _) =
  return "sub $4, %rsp\n \
\fstp 4(%rsp)\n"

toGasInstr (IrInstr Pop _ _) =
  return "add $4, %rsp\n"


toGasInstr (IrInstr Cmp op _) = do
  converted <- convertOperand op
  return $ "fcomp " ++ converted ++ "\n" ++
    "fstsw %ax\n\
    \fwait\n\
    \sahf\n"

toGasInstr (IrInstr opCode op0 EmptyOperand) = do
  code <- convertOpCode opCode
  converted0 <- convertOperand op0
  return $ code ++ " " ++ converted0 ++ "\n"

toGasInstr (IrInstr opCode op0 op1) = do
  code <- convertOpCode opCode
  converted0 <- convertOperand op0
  converted1 <- convertOperand op1
  return $ code ++ " " ++ converted1 ++ ", " ++ converted0 ++ "\n"

-- |The 'library' to insert at the top (i.e. the print function)
lib :: String
lib =
  ".data\n\
  \__hl_print_template: .ascii \"%f\\0\"\n\
  \__hl_println_template: .ascii \"%f\\n\\0\"\n\
  \.text\n \
  \print:\n \
  \push %rbp\n \
  \mov %rsp, %rbp\n \
  \and $-16, %rsp\n \
  \mov $1, %rax\n \
  \mov $__hl_print_template, %rdi\n \
  \call printf\n \
  \leave\n \
  \ret\n \
  \println:\n \
  \push %rbp\n \
  \mov %rsp, %rbp\n \
  \and $-16, %rsp\n \
  \mov $1, %rax\n \
  \mov $__hl_println_template, %rdi\n \
  \call printf\n \
  \leave\n \
  \ret\n"

-- |Using the symbol table, calculate the size of space that needs to be
-- allocated.
calcVarStackFrameSize :: State SymbolTable Int
calcVarStackFrameSize = do
  table <- SymbolTable.assocs
  onlyLocations <- return
                   $ map (\(k, v) -> case v of SymbolTable.Variable loc -> loc)
                   $ filter (\(k, v) -> case v of
                         SymbolTable.Variable v -> True
                         _ -> False) table
  largest <- return $ maximum onlyLocations
  return $ largest * 4

-- |Given a list of IrInstrs (for the .data section) and another list (for
-- .text), synthesise some asm.
synthesise :: [IrInstr] -> [IrInstr] -> State SymbolTable String
synthesise dataIr ir = do
  dataInstructions <- mapM toGasInstr dataIr
  instructions <- (mapM toGasInstr ir)
  -- Create instruction to allocate the stack for the local vars
  stackSize <- calcVarStackFrameSize
  allocStack <- return $ "sub $" ++ (show stackSize) ++ ", %rsp\n"
  main <- return $ [".globl main\nmain:\n \
                    \push %rbp\n \
                    \mov %rsp, %rbp\n"
                    ++ allocStack
                    ++"and $-16, %rsp\n"] ++
          instructions ++ ["leave\nret\n"]
  dataSection <- return $ ".data\n":dataInstructions
  textSection <- return $ ".text\n":main
  return $ foldl (++) [] (lib:dataSection ++ textSection)
