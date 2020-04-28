{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module Compiler
    ( codegenGlob,
      codegenBlock,
      codegenBlocks,
      Genv(..),
      CgMonad,
      CompiledBlock
    ) where

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.List as List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.Sequence as Seq (lookup, fromList)
import qualified Data.Word as Word

import qualified MicroRAM as MRAM
import qualified LLVM.AST.IntegerPredicate as IntPred


{- The assumptions:
   - We assume that register allocation has been performed.
     In that sense, this is malformed LLVM because it is not SSA.
   - Locations will represent registers according to a parameter
     name2register :: Name -> Maybe Reg
   - Special registers:,
     + 0. Accumulator register (AX). Used in arithmetic operations
     + 1. Counter register (CX). Used in shift/rotate instructions and loops.
     + 2. Data register (DX). Used in arithmetic operations and I/O operations.
     + 3. Base register (BX). Used as a pointer to data (located in segment register DS, when in segmented mode).
     + 4. Stack Pointer register (SP). Pointer to the top of the stack.
     + 5. Stack Base Pointer register (BP). Used to point to the base of the stack.
     + 6. Source Index register (SI). Used as a pointer to a source in stream operations.
     + 7. Destination Index register (DI). Used as a pointer to a destination in stream operations.
-}

-- ## Registers (see assumptions above)
ax = 0::Int
cx = 1::Int
dx = 2::Int
bx = 3::Int
sp = 4::Int
bp = 5::Int
si = 6::Int
di = 7::Int


-- This parameter should be passed by the register allocator
-- Here I show one hackt instantiation that only works for at most (length ['a'..'z'])  
regNum :: Int
regNum = 16

allWords :: [[Char]]
allWords =  [""] ++ [a:b | b<-allWords, a<-['a'..'z']]  
regNames = take regNum allWords

string2int :: String -> Maybe Int
string2int = flip List.elemIndex regNames

name2register :: LLVM.Name -> Maybe Reg
name2register (LLVM.Name name) = string2int (C8.unpack (Short.fromShort name))
name2register _ = Nothing

register2string:: Reg -> Maybe String
register2string r = Seq.lookup r (Seq.fromList regNames) 
register2name :: Reg -> Maybe LLVM.Name
register2name r = LLVM.Name <$> Short.toShort <$> C8.pack <$> (register2string r)


type ($) a b = a b

type Reg = Int
type Wrd = Int
type Ptr = Wrd
type Prog = MRAM.Program Reg Wrd

-- Error handling
data CgError = NotImpl String      -- Feature not implemented
           | CompilerAssumption String   -- Compiler assumption broken
           | OtherError String   -- Other error, stores the problem description.

-- Converts Error to a readable message.
instance Show CgError where
  show (NotImpl msg) =
      "The following feature has not been yet supported by the compiler: " ++ msg
  show (CompilerAssumption msg) = "Compiler assumption broken: " ++ msg
  show (OtherError msg) = msg

type CgMonad = Either CgError
implError msg = Left $ NotImpl msg
assumptError msg = Left $ CompilerAssumption msg
otherError msg = Left $ OtherError msg


-- MicroRAM   code generation


-- ## Compiling instructions

int2integer:: Int -> Integer
int2integer x = fromIntegral x

integer2int:: Integer -> CgMonad $ Int
integer2int x
  | x > (int2integer minBound) && x < (int2integer maxBound) = Right $ fromInteger x
  | otherwise = Left $ OtherError "Literal out of bounds" 

  
getConstant :: LLVM.Constant.Constant -> CgMonad $ Wrd
getConstant (LLVM.Constant.Int _ val) = integer2int val
getConstant _ = Left $ OtherError
  "Illegal constant. Maybe you used an unsuported type (e.g. float) or you forgot to run constant propagation (i.e. constant expresions in instructions)"

getReg :: LLVM.Name -> CgMonad $ Reg
getReg n = case name2register n of
              Just x -> Right x
              Nothing -> Left $ OtherError $ "Couldn't find register. "

getConstReg :: LLVM.Operand -> CgMonad $ Either Reg Wrd
getConstReg (LLVM.ConstantOperand c) = Right <$> getConstant c
getConstReg (LLVM.LocalReference _ name) = Left <$> getReg name
getConstReg _ = implError "operand, probably metadata"

type BinopInstruction = Reg -> Reg -> Either Reg Wrd -> MRAM.Instruction Reg Wrd
codegenBinop :: Genv -> Maybe Reg -> LLVM.Operand -> LLVM.Operand -> BinopInstruction -> CgMonad $ Prog
codegenBinop _ Nothing _ _ _ = Right $ [] --  without return is a noop
codegenBinop _ _ (LLVM.ConstantOperand _) (LLVM.ConstantOperand _) _ =
  Left $ CompilerAssumption "Two constants in a binop. Did you forget to run constant propagation?"
codegenBinop _ (Just ret) (LLVM.LocalReference _ name1) op2 bop =
  let r1 = getReg name1 in
    let a = getConstReg op2 in
     (\x -> [x]) <$> ((bop ret) <$> r1 <*> a)
codegenBinop _ _ _ _ _ = Left $ NotImpl "Binary operation with operands other than (register,register) or (register,constant)"

codegenInstruction :: Genv -> Maybe Reg -> LLVM.Instruction -> CgMonad $ Prog

fError = implError "Floatin point arithmetic"
uError = implError "unsigned operations"


constzero = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 0) 0)
constOne = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 1) 1)

(<:>):: Applicative f => f a -> f [a] -> f [a]
a <:> b = (:) <$> a <*> b

(<++>) :: Applicative f => f [a] -> f [a]  -> f [a] 
a <++> b = (++) <$> a <*> b

-- ## Arithmetic
-- Add

codegenInstruction genv ret (LLVM.Add _ _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Iadd
-- Sub
codegenInstruction genv ret (LLVM.Sub _ _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Isub
-- Mul
codegenInstruction genv ret (LLVM.Mul _ _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Imull
-- SDiv
codegenInstruction genv ret (LLVM.SDiv _ _ o1 o2 ) = implError "Signed division ius hard! SDiv"
-- SRem
codegenInstruction genv ret (LLVM.SRem o1 o2 _) = implError "Signed division ius hard! SRem"



-- ## Floating Point 
-- FAdd
codegenInstruction genv ret (LLVM.FAdd _ o1 o2 _) = implError "Fast Multiplication FMul"
-- FSub
codegenInstruction genv ret (LLVM.FSub _ o1 o2 _) =  implError "Fast Multiplication FMul"
-- FMul
codegenInstruction genv ret (LLVM.FMul _ o1 o2 _) =  implError "Fast Multiplication FMul"
-- FDiv
codegenInstruction genv ret (LLVM.FDiv _ o1 o2 _) =  implError "Fast Division FDiv"
-- FRem
codegenInstruction genv ret (LLVM.FRem _ o1 o2 _) = fError

-- ## Unsigned operations
-- UDiv
codegenInstruction genv ret (LLVM.UDiv _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Iudiv -- this is easy
-- URem
codegenInstruction genv ret (LLVM.URem o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Iumod -- this is eay


-- ##Shift operations
-- Shl
codegenInstruction genv ret (LLVM.Shl _ _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Ishl
-- LShr
codegenInstruction genv ret (LLVM.LShr _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Ishr
-- AShr
codegenInstruction genv ret (LLVM.AShr _ o1 o2 _) =  implError "Arithmetic shift right AShr"

-- ##Logical
--And
codegenInstruction genv ret (LLVM.And o1 o2 _) =  codegenBinop genv ret o1 o2 MRAM.Iand
--Or
codegenInstruction genv ret (LLVM.Or o1 o2 _) =  codegenBinop genv ret o1 o2 MRAM.Ior
--Xor
codegenInstruction genv ret (LLVM.Xor o1 o2 _) =  codegenBinop genv ret o1 o2 MRAM.Ixor

-- ## Memory operations
-- Alloca
{- we ignore type, alignment and metadata and assume we are storing integers,
   we only look at the numElements.
   In fact (currently) we only dont check  stack overflow or any similar safety check
   Also the current granularity of our memory is per word so ptr arithmetic and alignment are trivial. -}
codegenInstruction genv ret (LLVM.Alloca a Nothing b c) =
  codegenInstruction genv ret (LLVM.Alloca a (Just constOne) b c) --NumElements is defaulted to be one. 
--codegenInstruction genv ret (LLVM.Alloca _ (Just 0) _ _) = -- legal call but optimization possible
codegenInstruction genv (Just ret) (LLVM.Alloca a (Just n) b c) = (Right $ MRAM.Imov ret (Left sp)) <:>
                                                                   codegenInstruction genv Nothing (LLVM.Alloca a (Just n) b c)
codegenInstruction genv Nothing (LLVM.Alloca _ (Just n) _ _) =
  let a = getConstReg n in
    (MRAM.Iadd sp sp <$> a) <:> (Right [])

-- Load
codegenInstruction genv Nothing (LLVM.Load _ _ _ _ _) = Right [] -- Optimization reconside if we care about atomics
codegenInstruction genv (Just ret) (LLVM.Load _ n _ _ _) =
  let a = getConstReg n in
    (MRAM.Iload ret <$> a) <:> (Right []) 
-- Store
{- Store yields void so we can will ignore the return location -}
codegenInstruction genv _ (LLVM.Store _ adr val _ _ _) = 
  let cont = getConstReg val in
    let loc = getConstReg adr in
      loader <$> loc <*> cont
      where loader loc (Left reg) = [MRAM.Istore loc reg]
            loader loc cont@(Right val) = [MRAM.Imov ax cont, MRAM.Istore loc ax]


-- Compare
{- Unfortunately MRAM always puts a comparisons in teh "flag"
   So if we want the value we need to do 2 operations to get the value.
   In the future, we can reserve one "register" to work as the flag, then a smart
   register allocator can actually use the flag as it was intended...
-}
codegenInstruction genv Nothing (LLVM.ICmp pred op1 op2 _) =  Right [] -- Optimization
codegenInstruction genv (Just ret) (LLVM.ICmp pred op1 op2 _) =
  let lhs = getConstReg op1 in
    let rhs = getConstReg op2 in
      ((compare pred) <$> lhs <*> rhs >>= id) <:> cmptTail pred
      where cmptTail_pos = Right $ [MRAM.Imov ret (Left 0), MRAM.Icmov ret (Left 1)] -- moving the flag to the register... super wasteful! write a better register allocater
            cmptTail_neg = Right $ [MRAM.Imov ret (Left 1), MRAM.Icmov ret (Left 0)] -- Neg. the result && mov the flag to the register... wasteful! 
            cmptTail IntPred.EQ = cmptTail_pos -- This values allow to flip the result
            cmptTail IntPred.NE = cmptTail_neg 
            cmptTail IntPred.UGT = cmptTail_pos
            cmptTail IntPred.UGE = cmptTail_pos
            cmptTail IntPred.ULT = cmptTail_neg
            cmptTail IntPred.ULE = cmptTail_neg
            cmptTail IntPred.SGT = cmptTail_pos
            cmptTail IntPred.SGE = cmptTail_pos
            cmptTail IntPred.SLT = cmptTail_neg
            cmptTail IntPred.SLE = cmptTail_neg
            compare pred (Right lhs) _ = assumptError "Comparing left hand side constants (expected a register). Did you forget to do constant propagation?"
            compare IntPred.EQ (Left lhs) rhs = Right $ MRAM.Icmpe lhs rhs
            compare IntPred.NE (Left lhs) rhs = Right $ MRAM.Icmpe lhs rhs
            compare IntPred.SGT (Left lhs) rhs = Right $ MRAM.Icmpa lhs rhs
            compare IntPred.SGE (Left lhs) rhs = Right $ MRAM.Icmpae lhs rhs
            compare IntPred.SLT (Left lhs) rhs = Right $ MRAM.Icmpae lhs rhs
            compare IntPred.SLE (Left lhs) rhs = Right $ MRAM.Icmpa lhs rhs
            compare _ _ _ = implError "Unsigned comparisons"
                        

codegenInstruction genv _ (LLVM.Call _ _ _ _ _ _ _ ) =  implError $ "Call not implemented yet, but its next on my list"
codegenInstruction genv _ instr =  implError $ "Instruction: " ++ (show instr)




-- ## Named instructions

codegenNInstruction :: Genv -> LLVM.Named LLVM.Instruction -> CgMonad $ Prog
codegenNInstruction genv (LLVM.Do instr) = codegenInstruction genv Nothing instr
codegenNInstruction genv (name LLVM.:= instr) = let ret = getReg name in
                                                  (\ret -> codegenInstruction genv (Just ret) instr) =<< ret


-- ## Many instructions
codegenInstrs
  :: Genv
     -> [LLVM.Named LLVM.Instruction]
     -> CgMonad $ [MRAM.Instruction Reg Wrd]
codegenInstrs genv [] = Right $ []
codegenInstrs genv instrs = (foldr1 (++)) <$>  (mapM (codegenNInstruction genv) instrs)



-- ## Basic Blocks
{-
  LLVM.BasicBlock
  :: LLVM.Name
     -> [LLVM.Named LLVM.Instruction]
     -> LLVM.Named LLVM.Terminator
     -> LLVM.BasicBlock
Compiling basic blocks is done in three passes (can be optimized into two).
1. Compiles all the non terminator instructions.
2. Creates an mapping from block labels to instruction number.
3. Compiles the terminators using the mapping from labels to line no. to set the right target for control flow.

-}



data CompiledBlock = CompiledBlock { cb_name::LLVM.Name
                                   , cb_terminator::LLVM.Named LLVM.Terminator
                                   , cb_code::Prog
                                   } deriving (Eq, Read, Show)
type LabelMap = LLVM.Name -> Maybe Int
initLblMap :: LabelMap
initLblMap = \_ -> Nothing

(<--) :: LabelMap -> LLVM.Name -> Int -> LabelMap
(<--) lmap name n name' 
  | name == name' = Just n
  | otherwise = lmap name'

label2reg:: LabelMap -> LLVM.Name -> CgMonad $ Either Reg Wrd
label2reg lmap name =
  case lmap name of
    Just ln -> Right (Right ln)
    Nothing -> otherError $ "Label not found: " ++ show (name)

-- Statically we can know the numer of instructions taken to execute a terminator
-- Be carefull to update this when more terminators are implemented
-- Again this can be done much better if optimized into one pass
terminatorSize :: Num p => LLVM.Terminator -> p
terminatorSize (LLVM.Br name _) = 1
terminatorSize (LLVM.CondBr _ _ _ _) = 3
terminatorSize _ = 0

nTerminatorSize :: Num p => LLVM.Named LLVM.Terminator -> p
nTerminatorSize (_ LLVM.:= term) = terminatorSize term
nTerminatorSize (LLVM.Do term) = terminatorSize term

getNameMap_rec :: Int -> LabelMap -> [CompiledBlock] -> LabelMap
getNameMap_rec _ lmap [] = lmap
getNameMap_rec n lmap ((CompiledBlock name term code ):ls) =
  getNameMap_rec (n + (length code) + (nTerminatorSize term) ) (lmap <-- name $ n) ls

getNameMap :: [CompiledBlock] -> LabelMap
getNameMap = getNameMap_rec 0 initLblMap

-- We ignore the name of terminators
codegenTerminator :: LabelMap -> LLVM.Named LLVM.Terminator -> CgMonad $ Prog
codegenTerminator lmap (_ LLVM.:= term) = codegenTerminator' lmap term
codegenTerminator lmap (LLVM.Do term) = codegenTerminator' lmap term

codegenTerminator' :: LabelMap -> LLVM.Terminator -> CgMonad $ Prog
-- Branching
codegenTerminator' lmap (LLVM.Br name _) = (MRAM.Ijmp <$> (label2reg lmap name)) <:> (Right [])
codegenTerminator' lmap (LLVM.CondBr (LLVM.LocalReference _ name ) name1 name2 _) =
  let r1 = getReg name1 in
    let loc1 = label2reg lmap name1 in
      let loc2 = label2reg lmap name2 in
    ((flip MRAM.Icmpe (Left 1)) <$> r1) <:>
    ((MRAM.Icjmp <$> loc1) <:> 
    ((MRAM.Ijmp <$> loc2) <:>
    (Right [])))
codegenTerminator' lmap (LLVM.CondBr _ name1 name2 _) =
  assumptError "conditional branching must depend on a register. If you passed a constant prhaps you forgot to run constant propagation. Metadata is not supported."
codegenTerminator' lmap (LLVM.Ret _ _ ) = implError $ "Return not supported yet ... but its next on my list"
codegenTerminator' _ term = implError $ "Terminator not yet supported" ++ (show term)
      
  
codegenBlock :: Genv -> LLVM.BasicBlock -> CgMonad $ CompiledBlock
codegenBlock genv (LLVM.BasicBlock name instrs term) =
  (CompiledBlock name term) <$> (codegenInstrs genv instrs)

codegenCompiledBlock
  :: Genv
     -> LabelMap
     -> CompiledBlock
     -> CgMonad $ Prog
codegenCompiledBlock genv lmap (CompiledBlock name term prog) =
  (prog ++) <$> (codegenTerminator lmap term)


codegenBlocks :: Genv -> [LLVM.BasicBlock] -> CgMonad $ Prog
codegenBlocks genv blocks =
  let compiledBlocks = mapM (codegenBlock genv) blocks in
    let lmap = getNameMap <$> compiledBlocks in
      (foldr1 (++)) <$> (mapM <$> (((codegenCompiledBlock genv) <$> lmap)) <*> compiledBlocks >>= id)

-- ## Globals

codegenGlob :: Genv -> LLVM.Global -> CgMonad $ Prog
codegenGlob genv (LLVM.GlobalVariable name _ _ _ _ _ _ _ _ _ _ _ _ _) = Left $ NotImpl "Global Varianbles"
codegenGlob genv (LLVM.GlobalAlias name _ _ _ _ _ _ _ _) = Left $ NotImpl "Global Alias"
codegenGlob genv (LLVM.Function _ _ _ _ _ retT name params _ _ _ _ _ _ bb _ _) =
  Right $ []



codegenModule :: LLVM.Module -> CgMonad $ Prog
codegenModule (LLVM.Module _ _ _ _ defs) = codegenDefs defs

data Genv = Genv {
  vars :: [Char] -> Ptr
  , fun :: [Char] -> Ptr
}

codegenDefs :: [LLVM.Definition] -> CgMonad $ Prog
codegenDefs [] = Right [] 
codegenDefs (d:ds) = Right []

codegenDef :: Genv -> LLVM.Definition -> CgMonad $ Prog
codegenDef genv (LLVM.GlobalDefinition glob) = Right []
codegenDef genv otherDef = Left $ NotImpl (show otherDef)

