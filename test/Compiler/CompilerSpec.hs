{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Compiler.CompilerSpec
    () where

import Compiler.Compiler
import qualified LLVM.AST as LLVM
import qualified MicroRAM.MicroRAM as MRAM
import MicroRAM.MRAMInterpreter
import LLVM.AST (Named(..))
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.IntegerPredicate as IntPred
import GHC.Word as Word
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.String as String
--import Data.Bits.Extras
--import Data.Sequence as Seq

import qualified Test.QuickCheck as QC





-- Setup an interpreter to run the output
{- We assume the output to be in the first register
   We fix 16 registers
   There is no termination instruction... so we run forever
   The interpreter must take a input for "fuel"
-}

-- Build arbitrary words 
w32 :: Integral a => a -> Word.Word32
w32 = fromIntegral

wz = w32 (0::Int)

-- types
ty = LLVM.VoidType



-- Operand class to make notation easier
instance String.IsString LLVM.Operand where
  fromString  a = LLVM.LocalReference ty (LLVM.Name (Short.toShort (C8.pack a)))

int2op n = LLVM.ConstantOperand (LLVM.Constant.Int wz n)

instance Num LLVM.Operand where
  a + b = int2op 0 --Yes this is all boggus...
  a - b = int2op 0
  a * b = int2op 0
  negate b = int2op 0
  abs b = int2op 0
  signum b = int2op 0
  fromInteger n = int2op n
  {-
class Operandy a where
  toOperand :: a -> LLVM.Operand

--   Constants
instance Operandy Integer where
  toOperand a = LLVM.ConstantOperand (LLVM.Constant.Int wz a)

--   Regs
instance Operandy String where
  toOperand a = LLVM.LocalReference ty (LLVM.Name (Short.toShort (C8.pack a)))
-}



-- LLVM instructions


-- ## Arith
(.+),(.-),(.*),(./) :: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
(.+) o1 o2 = LLVM.Add False False o1 o2 []
(.-) o1 o2 = LLVM.Sub False False o1 o2 []
(.*) o1 o2 = LLVM.Mul False False o1 o2 []
(./) o1 o2 = LLVM.SDiv False o1 o2 []

-- ## Logic
(.&), (.|), xor:: LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
(.&) o1 o2 = LLVM.And o1 o2 []
(.|) o1 o2 = LLVM.Or o1 o2 []
xor o1 o2 = LLVM.Xor o1 o2 []


-- ## Comparisons
(.==),(.>),(.><),(.>=),(.<),(.<=)::
  LLVM.Operand -> LLVM.Operand -> LLVM.Instruction
(.==) o1 o2 = LLVM.ICmp IntPred.EQ o1 o2 []
(.><) o1 o2 = LLVM.ICmp IntPred.NE o1 o2 []
(.>) o1 o2 = LLVM.ICmp IntPred.SGT o1 o2 []
(.>=) o1 o2 = LLVM.ICmp IntPred.SGE o1 o2 []
(.<) o1 o2 = LLVM.ICmp IntPred.SLT o1 o2 []
(.<=) o1 o2 = LLVM.ICmp IntPred.SLE o1 o2 []



-- ## Memory
alloc:: LLVM.Instruction
alloc = (LLVM.Alloca ty Nothing wz [])

allocN:: LLVM.Operand -> LLVM.Instruction
allocN n = (LLVM.Alloca ty (Just n) wz [])

load:: LLVM.Operand -> LLVM.Instruction
load o1 = (LLVM.Load False o1 Nothing wz [])

-- ## Blocks
label
  :: LLVM.Name
     -> [Named LLVM.Instruction]
     -> Named LLVM.Terminator
     -> LLVM.BasicBlock
label name insts term =
  LLVM.BasicBlock name insts term

-- # Example 1
{- Adds 1 to "a" forever
-}
ex1 :: [LLVM.BasicBlock]
ex1 = [
  label "main" [
      "a":= ("a" .+ 1)]
    (Do $ LLVM.Br "loop" []),
    label "loop" [
      "a":= ("a" .+ 1),
      "":= ("a" .+ 0)]
    (Do $ LLVM.Br "loop" [])]
     
aa = ("a" .+ 1)

genv :: Genv
genv = Genv (\_ -> 0) (\_ -> 0)

ex1Comp = codegenBlocks genv ex1
test1 n = (flip execute (n) <$> ex1Comp)


-- # Example 2
{- fibonacci
-}
ex2 :: [LLVM.BasicBlock]
ex2 = [
  label "main" [ 
      "a":= ("a" .+ 1)]
    (Do $ LLVM.Br "loop" []),
    label "loop" [
      "b":= ("a" .+ 0),
      "a":= ("a" .+ ""),
      "":= ("b" .+ 0)]
    (Do $ LLVM.Br "loop" [])]
     
ex2Comp = codegenBlocks genv ex2
test2 n = (flip execute (4+n*4) <$> ex2Comp) -- returns fib n

