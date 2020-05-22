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
    flag_trace) where

import MicroRAM.MicroRAM
import Data.Bits
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))

{-
notes:
* Current implementation uses only signed integers (Int)
  but it supports unsigned integers (by converting them on the fly).
  We might want to change that to Data.Word so we can define the range
  and have better semantics of overflow?

-}

-- * MicroRAM semantics
{- In this semantics we represent words with integeres Int and
  registers are also indexed by intergers
-}
type Wrd = Int
type Reg = Int

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

type  Mem = Map.Map Wrd Wrd

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
  
init_state :: Int -> Tape -> Tape -> State
init_state k t_input t_advice  = State {
  pc = init_pc
  , regs = init_regs k
  , mem = init_mem
  , tapes = (t_input, t_advice)
  , flag = init_flag
  , bad = init_flag
}

set_reg:: State -> Reg -> Wrd -> State
set_reg st r x = State {
  pc = pc st
  , regs = Seq.update r x (regs st)
  , mem = mem st
  , tapes = tapes st
  , flag = flag st
  , bad = bad st
}

store_mem:: State -> Wrd -> Wrd -> State
store_mem st r x = State {
  pc = pc st
  , regs = regs st 
  , mem = store r x (mem st)
  , tapes = tapes st
  , flag = flag st
  , bad = bad st
}

set_flag:: State -> Bool -> State
set_flag st b = State {
  pc = pc st
  , regs = regs st 
  , mem = mem st
  , tapes = tapes st
  , flag = b
  , bad = bad st
}

set_pc:: State -> Wrd -> State
set_pc st pc' = State {
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
pop (x:tp) = Just ( x,tp)
pop _ = Nothing

set_tape::State -> Side -> Tape -> State
set_tape st sd tp =  State {
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
pop_tape::State -> Wrd -> Reg -> State
pop_tape st tp_n r =
  case try_pop_tape st tp_n r of
    Just st' -> set_flag st' False
    _ -> set_flag (set_reg st r 0) True
  where try_pop_tape st tp_n r = do
          sd <- to_side tp_n
          (x,tp) <- pop (get_tape st sd)
          Just $ set_reg (set_tape st sd tp) r x

next:: State -> State
next st = set_pc st (succ $ pc st )

-- * Interpreter

-- ** Utility evaluators

-- *** unary and binart operations

get_reg :: Regs -> Reg -> Wrd
get_reg rs r = Seq.index rs r 

get_either :: Regs -> Either Reg Wrd -> Wrd
get_either rs (Left r) = get_reg rs r
get_either rs (Right w) = w

bop :: Regs -> Reg -> Either Reg Wrd -> (Wrd -> Wrd -> x) -> x
bop rs r1 a f = f (get_reg rs r1) (get_either rs a)

uop :: Regs -> Either Reg Wrd -> (Wrd -> x) -> x
uop rs a f = f (get_either rs a)

exec_bop :: State -> Reg -> Reg -> Either Reg Wrd -> (Wrd -> Wrd -> Wrd) -> State
exec_bop st r1 r2 a f = next $ set_reg st r1 (bop (regs st) r2 a f)

exec_uop :: State -> Reg -> Either Reg Wrd -> ( Wrd -> Wrd) -> State
exec_uop st r1 a f = next $ set_reg st r1 (uop (regs st) a f)

-- *** Conditionals Util

exec_cnd :: State -> Reg -> Either Reg Wrd -> ( Wrd -> Wrd -> Bool) -> State
exec_cnd st r1 a f = next $ set_flag st (bop (regs st) r1 a f)

-- *** Jump util

exec_jmp st a = set_pc st (get_either (regs st) a)

-- ** Instruction execution (after instr. fetching)

exec :: Instruction Reg Wrd -> State -> State
exec (Iand r1 r2 a) st = exec_bop st r1 r2 a (.&.)
exec (Ior r1 r2 a) st = exec_bop st r1 r2 a (.|.)
exec (Ixor r1 r2 a) st = exec_bop st r1 r2 a xor
exec (Inot r1 a) st = exec_uop st r1 a complement

exec (Iadd r1 r2 a) st = exec_bop st r1 r2 a (+)
exec (Isub r1 r2 a) st = exec_bop st r1 r2 a (-)
exec (Imull r1 r2 a) st = exec_bop st r1 r2 a (*)
-- exec (Iumulh r1 r2 a) st = exec_bop st r1 r2 a (*)
-- exec (Iumulh r1 r2 a) st = exec_bop st r1 r2 a (*)
exec (Iudiv r1 r2 a) st = exec_bop st r1 r2 a quot
exec (Iumod r1 r2 a) st = exec_bop st r1 r2 a mod

exec (Ishl r1 r2 a) st = exec_bop st r1 r2 a shiftL
exec (Ishr r1 r2 a) st = exec_bop st r1 r2 a shiftR


exec (Icmpe r1 a) st = exec_cnd st r1 a (==)
exec (Icmpa r1 a) st = exec_cnd st r1 a (>)
exec (Icmpae r1 a) st = exec_cnd st r1 a (>=)
exec (Icmpg r1 a) st = exec_cnd st r1 a (>)
exec (Icmpge r1 a) st = exec_cnd st r1 a (>=)
 

exec (Ijmp a) st = exec_jmp st a
exec (Icjmp a) st = if flag st then exec_jmp st a else next st
exec (Icnjmp a) st = if not $ flag st then exec_jmp st a else next st

exec (Istore a r1) st = next $ store_mem st (get_either (regs st) a) (get_reg (regs st) r1)
exec (Iload r1 a) st = next $ set_reg st r1 (mem st ! (get_either (regs st) a))
exec (Iread r1 a) st = next $ pop_tape st (get_either (regs st) a) r1

-- ** Program step
type Prog = Program Reg Wrd

step :: Prog -> State -> State
step prog st = exec (prog !! pc st) st


-- ** Execution
type Trace = [State]
run :: Int -> Tape -> Tape -> Prog -> Trace
run k x w prog = (init_state k x w):(map (step prog) $ run k x w prog)


-- ** Some facilities to run
-- Simple getters to explore the trace.
get_regs :: State -> Regs
get_regs = regs

see_regs:: Trace -> Int -> Regs
see_regs t n = regs (t !! n)

reg_trace::Trace -> [Regs]
reg_trace t = map regs t

pc_trace'::Trace -> [Int]
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
