module MicroRAM.MRAMInterpreter
  ( Wrd,
    Reg,
    Regs,
    Mem,
    Tape,
    State(..),
    Prog,
    Trace,
    step,
    run,
    execute,
    exec_input,
    pc_trace,
    out_trace,
    flag_trace,
    toInt) where

import MicroRAM.MicroRAM
import Data.Bits
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))

{-
notes:
* Current implementation uses (Words) but it follows an interface
  that can be easily converted to any other Num
* Operations are performed over unbounded Integer and converted
  back anf forth from Wrd
* Follows the semantics in TinyRAM:
  https://www.scipr-lab.org/doc/TinyRAM-spec-0.991.pdf
-}

-- * MicroRAM semantics
{- In this semantics we represent words with Word and
  registers are indexed by intergers

 The interpreter is written such that it's easy
 to switch representations (e.e. Word8 or Int)
-}

type Wrd = Word
type Reg = Int
wrdMax = toInteger (maxBound :: Word)
wrdMin = toInteger (minBound :: Word)

toInt :: Integral a => a -> Int
toInt x = fromIntegral x

-- Most significant bit depends on implementation
-- If it's int then msb is the positive/negative marker
msb :: Wrd -> Bool 
msb x = x > (maxBound `quot` 2)

-- Some binary operations that are representation dependent

-- | Multiply and take the most significant bits.
umulh :: Integer -> Integer -> Integer
umulh r1 r2 = (r1 * r2) `quot` (wrdMax +1) -- this quotient removes the first W bits

-- | Multiply SIGNED and take the most significant bits.
-- We convert the operands through Int to interpret them as signed.
-- Then Multiply, then take the most significant bits  
smulh :: Integer -> Integer -> Integer
smulh r1 r2 = (r1' * r2') `quot` (wrdMax + 1)
  where r1' = toInteger $ toInt r1 -- By converting through Int, int's interpreted as signed.
        r2' = toInteger $ toInt r2  -- By converting through Int, int's interpreted as signed.


-- ** Program State

-- The Program Counter is a special register, represented separatedly 
type Pc = Wrd
init_pc = 0

-- | Registers
-- We represent the registers a a list of words, 
type Regs = Seq.Seq Wrd

init_regs :: Int -> Regs
init_regs k = Seq.replicate k 0 

-- The condition and bad flags
{- Current implementation of the flag only works for conditionals
 it does not get set on binary operations (e.g. on overflow) as in tiniyRAM

 We add a bad flag to be raised when an operation goes wrong. If the flag is
 set, the the rest of the state is bogus.
-}

init_flag = False

-- | Memory
-- Memory used to be
-- > Mem::Wrd -> Wrd 
-- but that is not good to building a (finite) trace
-- also we want programs that read uninitialized memory to bad

type Mem = Map.Map Wrd Wrd

init_mem :: Mem
init_mem = Map.empty

store ::  Wrd -> Wrd -> Mem -> Mem
store =  Map.insert

-- *** Tapes
type Tape = [Wrd]  -- ^read only tape

-- ** Program state State
{- I don't include the program in the state since it never changes

-}

-- | The program state 
data State = State {
  pc :: Pc
  , regs :: Regs
  , mem :: Mem
  , tapes :: (Tape, Tape)
  , flag :: Bool
  , bad :: Bool }
  deriving (Eq, Read, Show)
  
  
init_state :: Int -> Tape -> Tape -> State
init_state k t_input t_advice  = State {
  pc = init_pc
  , regs = init_regs k
  , mem = init_mem
  , tapes = (t_input, t_advice)
  , flag = init_flag
  , bad = init_flag
}

set_reg:: Reg -> Wrd -> State -> State
set_reg r x st = State {
  pc = pc st
  , regs = Seq.update r x (regs st)
  , mem = mem st
  , tapes = tapes st
  , flag = flag st
  , bad = bad st
}

store_mem::  Wrd -> Wrd -> State -> State
store_mem r x st = State {
  pc = pc st
  , regs = regs st 
  , mem = store r x (mem st)
  , tapes = tapes st
  , flag = flag st
  , bad = bad st
}

set_flag:: Bool -> State -> State
set_flag b st = State {
  pc = pc st
  , regs = regs st 
  , mem = mem st
  , tapes = tapes st
  , flag = b
  , bad = bad st
}

set_pc:: Wrd -> State -> State
set_pc pc' st = State {
  pc = pc'
  , regs = regs st 
  , mem = mem st
  , tapes = tapes st
  , flag = flag st
  , bad = bad st
}

-- Turn on the bad flag
-- there is no way to "unbad" a state
set_bad:: State -> State
set_bad st= State {
  pc = pc st
  , regs = regs st 
  , mem = mem st
  , tapes = tapes st
  , flag = flag st
  , bad = True
}
data Side = LeftSide | RightSide

get_pair::Side -> (a,a) -> a
get_pair LeftSide = fst
get_pair RightSide = snd

set_pair::Side -> a -> (a,a) -> (a,a)
set_pair LeftSide a (_, b)= (a, b)
set_pair RightSide b (a, _) = (a,b)

get_tape::State -> Side -> Tape
get_tape st b = get_pair b (tapes st)

to_side:: Wrd -> Maybe Side
to_side 0 = Just LeftSide
to_side 1 = Just RightSide
to_side _ = Nothing

pop::Tape -> Maybe (Wrd, Tape)
pop (x:tp) = Just (x,tp)
pop _ = Nothing

set_tape::Side -> Tape -> State -> State
set_tape sd tp st  =  State {
  pc = pc st
  , regs = regs st
  , mem = mem st
  , tapes = set_pair sd tp (tapes st)
  , flag = False -- ^ changing the tape always sets the flag to 0 (as per Tiny RAM semantics)
  , bad = bad st
}

-- Pop tape tries to pop a value from tape tp_n and store it in register r
-- if the tape is empty (or tp_n > 2) set r = 0 and flag = 1
-- if success set flag = 0
pop_tape::Wrd -> Reg -> State -> State
pop_tape tp_n r st =
  case try_pop_tape st tp_n r of
    Just st' -> set_flag False st'
    _ -> set_flag True (set_reg r 0 st)
  where try_pop_tape st tp_n r = do
          sd <- to_side tp_n
          (x,tp) <- pop (get_tape st sd)
          Just $ set_reg r x $ set_tape sd tp st

next:: State -> State
next st = set_pc (succ $ pc st) st

-- * Interpreter

-- ** Utility evaluators

-- | Register getters (from a set register set)
get_reg :: Regs -> Reg -> Wrd
get_reg rs r = Seq.index rs r 

eval_reg st r = get_reg (regs st) r

-- | Gets operand wether it's a register or a constant or a PC
eval_operand :: State -> Operand Reg Wrd -> Wrd
eval_operand st (Reg r) = eval_reg st r
eval_operand st (Const w) = w

-- *** unary and binart operations
{- The way we computeto do binary/unary operations we do the following steps:
   1 - Compute the operands (results are type Wrd)
   2 - Transforms the operands to Integer
   3 - Compute the operation over the integers
   4 - Transform the result to Wrd and store it in the return register
   5 - Set the flag, if the given condition is satisfied over the result

We use Integers to be homogeneus over all possible types Wrd and because it makes checking under/overflow easier
-}

-- | Binary operations generic.
bop :: State
       -> Reg
       -> Operand Reg Wrd
       -> (Integer -> Integer -> x) -- ^ Binary operation
       -- -> (Integer -> Bool) -- ^ Set the flag? Is applied to the result of the operation 
       -> x
bop rs r1 a f = f (toInteger $ get_reg (regs rs) r1) (toInteger $ eval_operand rs a)

-- | Unart operations generic. 
uop :: State
       -> Operand Reg Wrd
       -> (Integer -> x)
       -- -> (Integer -> Bool) -- ^ Set the flag? Is applied to the result of the operation 
       -> x
uop rs a f = f (toInteger $ eval_operand rs a)


-- | Catches division by 0
-- By TinyRAM semantics, this sets the flag to 0 and returns 0
-- I would like to flag this as an error.
exception :: Bool
          -> Reg
          -> (State -> State) -- ^ continuation
          -> State
          -> State
exception False _ f st = f st
exception True r _ st = set_flag True $ set_reg r 0 st
catchZero :: Wrd
           -> Reg
          -> (State -> State) -- ^ continuation
          -> State
          -> State
catchZero w = exception (w == 0)

exec_bop :: State
         -> Reg
         -> Reg
         -> Operand Reg Wrd
         -> (Integer -> Integer -> Integer) -- ^ Binary operation
         -> (Integer -> Bool) -- ^ Checks if flag should be set
         -> State 
exec_bop st r1 r2 a f check = next $ set_flag (check result) $ set_reg r1 (fromInteger result) st
  where result = bop st r2 a f

-- | Evaluate binop, but first check a<>0 
execBopCatchZero :: State
         -> Reg
         -> Reg
         -> Operand Reg Wrd
         -> (Integer -> Integer -> Integer) -- ^ Binary operation
         -> State 
execBopCatchZero st r1 r2 a f =
  catchZero (eval_operand st a) r1 (\st -> exec_bop st r1 r2 a quot (\_->True)) st 


exec_uop :: State -> Reg -> Operand Reg Wrd
         -> (Integer -> Integer) -- ^ Unary operatio
         -> (Integer -> Bool) -- ^ Checks if flag should be set
         -> State
exec_uop st r1 a f check = next $ set_flag (check result) $ set_reg r1 (fromInteger result) st
  where result = uop st a f


-- Common checks for binary operations (to set the flag)
isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False
notZero x = not (isZero x)


overflow :: Integer -> Bool
overflow i = i > wrdMax

borrow :: Integer -> Bool
borrow i = i < 0

overUnderflow :: Integer -> Bool
overUnderflow i = i < wrdMin || i > wrdMax


trivialCheck :: Integer -> Bool
trivialCheck _ = True

-- compute most/less significant digit
lsb :: Wrd -> Bool
lsb x = 0 == x `mod` 2
                 
-- *** Conditionals Util

exec_cnd :: State -> Reg -> Operand Reg Wrd -> (Integer -> Integer -> Bool) -> State
exec_cnd st r1 a f = next $ set_flag result st
                        where result = bop st r1 a f

-- *** Jump util

exec_jmp st a = set_pc (eval_operand st a) st

-- ** Instruction execution (after instr. fetching)

exec :: Instruction Reg Wrd -> State -> State
exec (Iand r1 r2 a) st = exec_bop st r1 r2 a (.&.) isZero
exec (Ior r1 r2 a) st = exec_bop st r1 r2 a (.|.) isZero
exec (Ixor r1 r2 a) st = exec_bop st r1 r2 a xor isZero
exec (Inot r1 a) st = exec_uop st r1 a complement isZero

exec (Iadd r1 r2 a) st = exec_bop st r1 r2 a (+) overflow
exec (Isub r1 r2 a) st = exec_bop st r1 r2 a (-) borrow
exec (Imull r1 r2 a) st = exec_bop st r1 r2 a (*) overflow

exec (Iumulh r1 r2 a) st = exec_bop st r1 r2 a umulh notZero -- flagged iff the return is not zero (indicates overflow)
exec (Ismulh r1 r2 a) st = exec_bop st r1 r2 a smulh notZero  -- flagged iff the return is not zero (indicates overflow)
exec (Iudiv r1 r2 a) st = execBopCatchZero st r1 r2 a quot
exec (Iumod r1 r2 a) st = execBopCatchZero st r1 r2 a rem

-- Shifts are a bit tricky since the flag depends on the operand not the result.
exec (Ishl r1 r2 a) st = set_flag (msb $ eval_reg st r1) $
  exec_bop st r1 r2 a (\a b -> shiftL a (fromInteger b)) trivialCheck
exec (Ishr r1 r2 a) st = set_flag (lsb $ eval_reg st r1) $
  exec_bop st r1 r2 a (\a b -> shiftR a (fromInteger b)) trivialCheck

-- Compare operations
exec (Icmpe r1 a) st = exec_cnd st r1 a (==)
exec (Icmpa r1 a) st = exec_cnd st r1 a (>)
exec (Icmpae r1 a) st = exec_cnd st r1 a (>=)
exec (Icmpg r1 a) st = exec_cnd st r1 a (>)
exec (Icmpge r1 a) st = exec_cnd st r1 a (>=)

-- Move operations
exec (Imov r a) st = next $ set_reg r (eval_operand st a) st
exec (Icmov r a) st = if flag st
  then exec (Imov r a) st
  else next st
 
-- Jump Operations
exec (Ijmp a) st = exec_jmp st a
exec (Icjmp a) st = if flag st then exec_jmp st a else next st
exec (Icnjmp a) st = if not $ flag st then exec_jmp st a else next st

--Memory operations
exec (Istore a r1) st = next $ store_mem (eval_operand st a) (get_reg (regs st) r1) st
exec (Iload r1 a) st = next $ set_reg r1 (mem st ! (eval_operand st a)) st
exec (Iread r1 a) st = next $ pop_tape (eval_operand st a) r1 st

exec (Ianswer a) st = set_reg 0 (eval_operand st a) st -- sets register 0 to the answer and loops (pc not incremented)

-- ** Program step
type Prog = Program Reg Wrd

step :: Prog -> State -> State
step prog st = exec (prog !! (toInt $ pc st)) st


-- ** Execution
type Trace = [State]
run :: Int -> Tape -> Tape -> Prog -> Trace
run k x w prog = iterate (step prog) $ init_state k x w

-- ** Some facilities to run
-- Simple getters to explore the trace.
get_regs :: State -> Regs
get_regs = regs

see_regs:: Trace -> Int -> Regs
see_regs t n = regs (t !! n)

reg_trace::Trace -> [Regs]
reg_trace t = map regs t

pc_trace'::Trace -> [Wrd]
pc_trace' t= map pc t

flag_trace'::Trace -> [Bool]
flag_trace' t= map flag t

-- Convenient execution
{- As long as we haven't implemented a "return",
   The return value will be stroed in the first register.
-}
k = 16

run' n prog = Prelude.take n (run k [] [] prog)
pc_trace n prog = map pc (run' n prog)
out_trace n prog = map (\s-> Seq.index (regs s) 0) (run' n prog)
flag_trace n prog = map flag (run' n prog)

execute :: Prog -> Int -> Wrd
execute prog n = Seq.index (regs $ (run k [] [] prog) !! n) 0

execute_pc prog n = Seq.index ((see_regs $ run k [] [] prog) n) 0

exec_input :: Prog -> Tape -> Tape -> Int -> Wrd
exec_input prog x w n = Seq.index ((see_regs $ run k x w prog) n) 0
