{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Compiler
Description : LLVM' -> MicroASM
Maintainer  : santiago@galois.com
Stability   : experimental

This module compiles LLVM' to MicroASM.

LLVM' is different from LLVM in that it has been register allocated. 
So every name represents a register from a finite set. In particular,
LLVM' is not in SSA form.


MicroASM is different to MicrRAM in that it allows the operands
`Label` and `HereLabel`. The assembler will replace those labels
with the actual instruction numbers to obtain MicroRAM. In particular
a MicroASM program can be "partial" and needs to be linked to another part
that contains some of the labels (this allows some simple separta compilation.

-}

module Compiler.CodeGenerator
    ( codeGen,
      codegenGlob,
      codegenBlock,
      codegenBlocks,
      Genv(..),
      CgMonad,
      CgError(..),
      CompiledBlock,
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

import MicroRAM.MicroRAM (Operand'(..), MAOperand) 
import qualified MicroRAM.MicroRAM as MRAM
import qualified LLVM.AST.IntegerPredicate as IntPred
import qualified LLVM.AST.ParameterAttribute as ParamAtt

{-| Notes on this compiler :
   - We assume that register allocation has been performed.
     In that sense, this is malformed LLVM because it is not SSA.
   - Locations will represent registers according to a parameter
     name2register :: Name -> Maybe Reg
   - Special registers (simulating x86, very simple)
     + 0. Accumulator register (AX). Used in arithmetic operations
     + 1. Counter register (CX). Used in shift/rotate instructions and loops.
     + 2. Data register (DX). Used in arithmetic operations and I/O operations.
     + 3. Base register (BX). Used as a pointer to data (located in segment register DS, when in segmented mode).
     + 4. Stack Pointer register (SP). Pointer to the top of the stack.
     + 5. Stack Base Pointer register (BP). Used to point to the base of the stack.
     + 6. Source Index register (SI). Used as a pointer to a source in stream operations.
     + 7. Destination Index register (DI). Used as a pointer to a destination in stream operations.
   - Function calls:
     + No tailcall optimizations
     + Calling conventions are ignored!
     + Parameter attributes are ignored
     + No exception handling (exception dest ignored)
     + All arguments are passed on the stack (room for optimization)
     + Stack is backwards! It's customary for SP to decrease, but we don't do that yet
     + Return is assumed to be a single Wrd
     + Stacks is layout:
     |                 |
     +=================+ 
     | Local variables | <- SP
     |                 |
     |                 |
     +-----------------+
     | Parameters      |
     |                 |
     +-----------------+
     | Return address  |
     +-----------------+ 
     | Old fram ptr    | <- BP
     +=================+
     | Caller frame    |
     |                 |
     (Return value is passed on regiter 0, not on stack
      Must return a single word. )


-}

-- * Util definitions

-- ** Registers (see assumptions above)
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

short2string = C8.unpack . Short.fromShort

name2string :: LLVM.Name -> Maybe String
name2string (LLVM.Name name) = Just $ short2string name
name2string _ = Nothing

name2register :: LLVM.Name -> Maybe Reg
name2register name = do
  str_name <- name2string name
  string2int str_name

register2string:: Reg -> Maybe String
register2string r = Seq.lookup r (Seq.fromList regNames) 
register2name :: Reg -> Maybe LLVM.Name
register2name r = LLVM.Name <$> Short.toShort <$> C8.pack <$> (register2string r)


type ($) a b = a b

type Wrd = Word
type Reg = Int
type Ptr = Wrd
type Prog = MRAM.MAProgram Reg Wrd

 -- ** Error handling
data CgError = NotImpl String      -- Feature not implemented
           | CompilerAssumption String   -- Compiler assumption broken
           | OtherError String   -- Other error, stores the problem description.

-- | Converts Error to a readable message.
instance Show CgError where
  show (NotImpl msg) =
      "The following feature has not been yet supported by the compiler: " ++ msg
  show (CompilerAssumption msg) = "Compiler assumption broken: " ++ msg
  show (OtherError msg) = msg

type CgMonad = Either CgError
implError msg = Left $ NotImpl msg
assumptError msg = Left $ CompilerAssumption msg
otherError msg = Left $ OtherError msg


-- ** Usefull snipets
push r = [MRAM.Istore (Reg sp) r, MRAM.Iadd sp sp (Const 1)]
pop r = [MRAM.Iload r (Reg sp), MRAM.Isub  sp sp (Const 1)]

-- ** Function calls
-- | Generate code for function call
type Parameter = (LLVM.Operand, [ParamAtt.ParameterAttribute])
funCodeGen :: Genv -> Maybe Reg -> LLVM.CallableOperand -> [Parameter] -> CgMonad $ [MRAM.MAInstruction Reg Wrd]
funCodeGen _ _ (Left _) _ = implError $ "Inlined assembly not supported"
funCodeGen genv ret (Right (LLVM.LocalReference ty nm)) param =
  funCodeGen' ret ty nm param 
funCodeGen _ _ (Right _) _ = implError $ "Functions can only be called by local references for now"

-- | Push parameters in the stack
pushParams ptys params = []

putResult :: Maybe Reg -> [MRAM.MAInstruction Reg Wrd]
puResult (Just r) = [MRAM.Imov r (Reg ax)]
putResult _ = []

funCodeGen' :: Maybe Reg -> LLVM.Type -> LLVM.Name -> [Parameter] -> CgMonad $ [MRAM.MAInstruction Reg Wrd]
funCodeGen' _ (LLVM.FunctionType  _ _ True) _ _ = implError $ "Variable parameters (isVarArg in function call)." 
funCodeGen' ret (LLVM.FunctionType resTy argTys False) nm params = 
  case nm of
    LLVM.UnName _ -> implError $ "Unnamed functions not supported."
    LLVM.Name fnm -> Right $
      -- Change functions frame 
      (push bp) ++
      [MRAM.Imov bp (Reg sp)] ++
      -- Push return addres
      [MRAM.Imov ax HereLabel, MRAM.Iadd ax ax (Const 1)] ++ -- Store return address 
      (push ax) ++
      (pushParams argTys params) ++
      [MRAM.Ijmp (Label $ short2string fnm)] ++
      -- Restore old call frame
      [MRAM.Imov sp (Reg bp)] ++  -- Restore stack top pointer
      (pop bp) ++                 -- Restore stack base pointer
      putResult ret              -- Stores the result in ret if any
                                  -- notice that result is already in ax
                                  -- so it is adviced to skip this step and use ax
         
funCodeGen' _ ty _ _ = assumptError $ "Function type expected found " ++ show ty ++ " instead."

-- * MicroRAM code generation


-- ** Compiling instructions

wrd2integer:: Wrd -> Integer
wrd2integer x = fromIntegral x

integer2wrd:: Integer -> CgMonad $ Wrd
integer2wrd x
  | x > (wrd2integer minBound) && x < (wrd2integer maxBound) = Right $ fromInteger x
  | otherwise = Left $ OtherError "Literal out of bounds" 

  
getConstant :: LLVM.Constant.Constant -> CgMonad $ Wrd
getConstant (LLVM.Constant.Int _ val) = integer2wrd val
getConstant _ = Left $ OtherError
  "Illegal constant. Maybe you used an unsuported type (e.g. float) or you forgot to run constant propagation (i.e. constant expresions in instructions)"

getReg :: LLVM.Name -> CgMonad $ Reg
getReg n = case name2register n of
              Just x -> Right x
              Nothing -> Left $ OtherError $ "Couldn't find register. "

getConstReg :: LLVM.Operand -> CgMonad $ MAOperand Reg Wrd
getConstReg (LLVM.ConstantOperand c) = Const <$> getConstant c
getConstReg (LLVM.LocalReference _ name) = Reg <$> getReg name
getConstReg _ = implError "operand, probably metadata"

type BinopInstruction = Reg -> Reg -> MRAM.MAOperand Reg Wrd -> MRAM.MAInstruction Reg Wrd
codegenBinop :: Genv -> Maybe Reg -> LLVM.Operand -> LLVM.Operand -> BinopInstruction -> CgMonad $ [MRAM.MAInstruction Reg Wrd]
codegenBinop _ Nothing _ _ _ = Right $ [] --  without return is a noop
codegenBinop _ _ (LLVM.ConstantOperand _) (LLVM.ConstantOperand _) _ =
  Left $ CompilerAssumption "Two constants in a binop. Did you forget to run constant propagation?"
codegenBinop _ (Just ret) (LLVM.LocalReference _ name1) op2 bop =
  let r1 = getReg name1 in
    let a = getConstReg op2 in
     (\x -> [x]) <$> ((bop ret) <$> r1 <*> a)
codegenBinop _ _ _ _ _ = Left $
  NotImpl "Binary operation with operands other than (register,register) or (register,constant)"

codegenInstruction :: Genv -> Maybe Reg -> LLVM.Instruction -> CgMonad $ [MRAM.MAInstruction Reg Wrd]

fError = implError "Floatin point arithmetic"
uError = implError "unsigned operations"


constzero = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 0) 0)
constOne = LLVM.ConstantOperand (LLVM.Constant.Int (toEnum 1) 1)

(<:>):: Applicative f => f a -> f [a] -> f [a]
a <:> b = (:) <$> a <*> b

(<++>) :: Applicative f => f [a] -> f [a]  -> f [a] 
a <++> b = (++) <$> a <*> b

-- *** Arithmetic

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



-- *** Floating Point 
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

-- *** Unsigned operations
-- UDiv
codegenInstruction genv ret (LLVM.UDiv _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Iudiv -- this is easy
-- URem
codegenInstruction genv ret (LLVM.URem o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Iumod -- this is eay


-- *** Shift operations
-- Shl
codegenInstruction genv ret (LLVM.Shl _ _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Ishl
-- LShr
codegenInstruction genv ret (LLVM.LShr _ o1 o2 _) = codegenBinop genv ret o1 o2 MRAM.Ishr
-- AShr
codegenInstruction genv ret (LLVM.AShr _ o1 o2 _) =  implError "Arithmetic shift right AShr"

-- *** Logical
--And
codegenInstruction genv ret (LLVM.And o1 o2 _) =  codegenBinop genv ret o1 o2 MRAM.Iand
--Or
codegenInstruction genv ret (LLVM.Or o1 o2 _) =  codegenBinop genv ret o1 o2 MRAM.Ior
--Xor
codegenInstruction genv ret (LLVM.Xor o1 o2 _) =  codegenBinop genv ret o1 o2 MRAM.Ixor

-- *** Memory operations
-- Alloca
{- we ignore type, alignment and metadata and assume we are storing integers,
   we only look at the numElements.
   In fact (currently) we only dont check  stack overflow or any similar safety check
   Also the current granularity of our memory is per word so ptr arithmetic and alignment are trivial. -}
codegenInstruction genv ret (LLVM.Alloca a Nothing b c) =
  codegenInstruction genv ret (LLVM.Alloca a (Just constOne) b c) --NumElements is defaulted to be one. 
--codegenInstruction genv ret (LLVM.Alloca _ (Just 0) _ _) = -- legal call but optimization possible
codegenInstruction genv (Just ret) (LLVM.Alloca a (Just n) b c) = (Right $ MRAM.Imov ret (Reg sp)) <:>
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
      where loader loc (Reg reg) = [MRAM.Istore loc reg]
            loader loc cont@(Const val) = [MRAM.Imov ax cont, MRAM.Istore loc ax]


-- *** Compare
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
      where cmptTail_pos = Right $ [MRAM.Imov ret (Reg 0), MRAM.Icmov ret (Reg 1)] -- moving the flag to the register... super wasteful! write a better register allocater
            cmptTail_neg = Right $ [MRAM.Imov ret (Reg 1), MRAM.Icmov ret (Reg 0)] -- Neg. the result && mov the flag to the register... wasteful! 
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
            compare pred (Const lhs) _ = assumptError "Comparing left hand side constants (expected a register). Did you forget to do constant propagation?"
            compare IntPred.EQ (Reg lhs) rhs = Right $ MRAM.Icmpe lhs rhs
            compare IntPred.NE (Reg lhs) rhs = Right $ MRAM.Icmpe lhs rhs
            compare IntPred.SGT (Reg lhs) rhs = Right $ MRAM.Icmpa lhs rhs
            compare IntPred.SGE (Reg lhs) rhs = Right $ MRAM.Icmpae lhs rhs
            compare IntPred.SLT (Reg lhs) rhs = Right $ MRAM.Icmpae lhs rhs
            compare IntPred.SLE (Reg lhs) rhs = Right $ MRAM.Icmpa lhs rhs
            compare _ _ _ = implError "Unsigned comparisons"
                        
-- *** Function Call 
codegenInstruction genv ret (LLVM.Call _ _ _ f args _ _ ) =
  funCodeGen genv ret f args
-- *** Not supprted instructions (return meaningfull error)
codegenInstruction genv _ instr =  implError $ "Instruction: " ++ (show instr)



{- 

    

-}


-- ** Named instructions and instructions lists

codegenNInstruction :: Genv -> LLVM.Named LLVM.Instruction -> CgMonad $ [MRAM.MAInstruction Reg Wrd]
codegenNInstruction genv (LLVM.Do instr) = codegenInstruction genv Nothing instr
codegenNInstruction genv (name LLVM.:= instr) = let ret = getReg name in
                                                  (\ret -> codegenInstruction genv (Just ret) instr) =<< ret
codegenInstrs
  :: Genv
     -> [LLVM.Named LLVM.Instruction]
     -> CgMonad $ [MRAM.MAInstruction Reg Wrd]
codegenInstrs genv [] = Right $ []
codegenInstrs genv instrs = (foldr1 (++)) <$>  (mapM (codegenNInstruction genv) instrs)



-- ** Basic Blocks
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
type CompiledBlock = MRAM.NamedBlock Reg Wrd
--data CompiledBlock = CompiledBlock { cb_name::Maybe String
--                                   , cb_code::[MRAM.MAInstruction Reg Wrd]
--                                   } deriving (Eq, Read, Show)
type LabelMap = LLVM.Name -> Maybe Int
initLblMap :: LabelMap
initLblMap = \_ -> Nothing

(<--) :: LabelMap -> LLVM.Name -> Int -> LabelMap
(<--) lmap name n name' 
  | name == name' = Just n
  | otherwise = lmap name'

label2operand:: LLVM.Name -> CgMonad $ MAOperand Reg Wrd
label2operand (LLVM.Name nm) = Right $ Label $ short2string nm
label2operand (LLVM.UnName _) = otherError $ "Unnammed labels not supported yet"

-- Statically we can know the numer of instructions taken to execute a terminator
-- Be carefull to update this when more terminators are implemented
-- Again this can be done much better if optimized into one pass
{-
terminatorSize :: Num p => LLVM.Terminator -> p
terminatorSize (LLVM.Br name _) = 1
terminatorSize (LLVM.CondBr _ _ _ _) = 3
terminatorSize _ = 0

nTerminatorSize :: Num p => LLVM.Named LLVM.Terminator -> p
nTerminatorSize (_ LLVM.:= term) = terminatorSize term
nTerminatorSize (LLVM.Do term) = terminatorSize term

getNameMap_rec :: Int -> LabelMap -> [CompiledBlock] -> LabelMap
getNameMap_rec _ lmap [] = lmap
getNameMap_rec n lmap ((CompiledBlock name code ):ls) =
  getNameMap_rec (n + (length code) + (nTerminatorSize term) ) (lmap <-- name $ n) ls

getNameMap :: [CompiledBlock] -> LabelMap
getNameMap = getNameMap_rec 0 initLblMap
-}
-- We ignore the name of terminators
codegenTerminator :: LLVM.Named LLVM.Terminator -> CgMonad $ [MRAM.MAInstruction Reg Wrd]
codegenTerminator (name LLVM.:= term) =  codegenTerminator' term
codegenTerminator (LLVM.Do term) = codegenTerminator' term

codegenTerminator' :: LLVM.Terminator -> CgMonad $ [MRAM.MAInstruction Reg Wrd]
-- Branching
codegenTerminator' (LLVM.Br name _) = (MRAM.Ijmp <$> (label2operand name)) <:> (Right [])
codegenTerminator' (LLVM.CondBr (LLVM.LocalReference _ name ) name1 name2 _) = do
  r1 <- getReg name
  loc1 <- label2operand name1
  loc2 <- label2operand name2
  Right $ [MRAM.Icmpe r1 (Const 1),
    MRAM.Icjmp loc1,
     MRAM.Ijmp loc2]
codegenTerminator' (LLVM.CondBr _ name1 name2 _) =
  assumptError "conditional branching must depend on a register. If you passed a constant prhaps you forgot to run constant propagation. Can't branch on Metadata."
codegenTerminator' (LLVM.Ret (Just ret) md) = do
  ret_val <- getConstReg ret
  cont <- codegenTerminator' (LLVM.Ret Nothing md)
  Right $ MRAM.Imov ax ret_val : cont 
codegenTerminator' (LLVM.Ret Nothing _) = Right $
  [MRAM.Iadd ax bp (Const 1),        -- ax = location of return addres
   MRAM.Iload ax (Reg ax),           -- ax = return address
   MRAM.Ijmp (Reg ax)]               -- Goto return location.
codegenTerminator' term = implError $ "Terminator not yet supported" ++ (show term)

codegenBlock :: Genv -> LLVM.BasicBlock -> CgMonad $ CompiledBlock
codegenBlock genv (LLVM.BasicBlock name instrs term) = do
  body <- codegenInstrs genv instrs
  end <- codegenTerminator term
  Right $ MRAM.NBlock (name2string name) $ body ++ end

{-
codegenCompiledBlock
  :: Genv
     -> CompiledBlock
     -> CgMonad $ [MRAM.MAInstruction Reg Wrd]
codegenCompiledBlock genv (CompiledBlock name prog) =
  (prog ++) <$> (codegenTerminator term)  -- Move this to the assembler!
-}

codegenBlocks :: Genv -> [LLVM.BasicBlock] -> CgMonad $ [CompiledBlock]
codegenBlocks genv blocks = mapM (codegenBlock genv) blocks

-- ## Globals

codegenGlob :: Genv -> LLVM.Global -> CgMonad $ [CompiledBlock]
codegenGlob genv (LLVM.GlobalVariable name _ _ _ _ _ _ _ _ _ _ _ _ _) = Left $ NotImpl "Global Variables"
codegenGlob genv (LLVM.GlobalAlias name _ _ _ _ _ _ _ _) = Left $ NotImpl "Global Alias"
codegenGlob genv (LLVM.Function _ _ _ _ _ retT name params _ _ _ _ _ _ code _ _) = do
  body <- codegenBlocks genv code
  let header = MRAM.NBlock (name2string name) [] in -- Empty block signals function entry.  
    Right $ header : body



codegenModule :: Genv ->  LLVM.Module -> CgMonad $ MRAM.MAProgram Reg Wrd
codegenModule ge (LLVM.Module _ _ _ _ defs) = codegenDefs ge defs

data Genv = Genv {
  vars :: [Char] -> Ptr
  , fun :: [Char] -> Maybe Int -- Instruction location
}
emptyGenv = Genv (\x -> 0) (\x -> Nothing)

codegenDefs :: Genv -> [LLVM.Definition] -> CgMonad $ MRAM.MAProgram Reg Wrd
codegenDefs ge ds = concat <$> mapM (codegenDef ge) ds

codegenDef :: Genv -> LLVM.Definition -> CgMonad $ MRAM.MAProgram Reg Wrd
codegenDef genv (LLVM.GlobalDefinition glob) = Right []
codegenDef genv otherDef = Left $ NotImpl (show otherDef)


codeGen = codegenModule emptyGenv
