module MRAMInterpreter
  ( Wrd,
    Reg,
    Regs,
    Mem,
    Tape,
    State(..),
    Prog,
    Trace,
    step,
    run) where

import MicroRAM
import Data.Bits
import Data.Sequence as Seq

{-
Notes:

* Current implementation uses only signed integers (Int)
  for unsigned use Data.Word
-}


-- # One implementation

type Wrd = Int
type Reg = Int


-- ## The Program Counter
type Pc = Wrd
init_pc = 0

-- ## Registers
type Regs = Seq Wrd

init_regs :: Int -> Regs
init_regs k = Seq.replicate k 0 

-- ## The condition flag
{- Current implementation of the flag only works for conditionals
 it does not get set on binary operations (e.g. on overflow)
 as in tiniyRAM
-}
init_flag = False

-- ## Memory
type  Mem = Wrd -> Wrd

init_mem :: Mem
init_mem = \_ -> 0

store :: Mem -> Wrd -> Wrd -> Mem
store m l a = \x -> if l == x then a else m x

-- ## Tapes
type Tape = [Wrd]

-- ## State
{- I don't include the program in the state since it never changes -}
data State = State {
  pc :: Pc
  , regs :: Regs
  , mem :: Mem
  , tapes :: (Tape, Tape)
  , flag :: Bool}
  
init_state :: Int -> Tape -> Tape -> State
init_state k t_input t_advice  = State {
  pc = init_pc
  , regs = init_regs k
  , mem = init_mem
  , tapes = (t_input, t_advice)
  , flag = init_flag
}

set_reg:: State -> Reg -> Wrd -> State
set_reg st r x = State {
  pc = pc st
  , regs = update r x (regs st)
  , mem = mem st
  , tapes = tapes st
  , flag = flag st
}

store_mem:: State -> Wrd -> Wrd -> State
store_mem st r x = State {
  pc = pc st
  , regs = regs st 
  , mem = store (mem st) r x
  , tapes = tapes st
  , flag = flag st
}

set_flag:: State -> Bool -> State
set_flag st b = State {
  pc = pc st
  , regs = regs st 
  , mem = mem st
  , tapes = tapes st
  , flag = b
}

set_pc:: State -> Wrd -> State
set_pc st pc' = State {
  pc = pc'
  , regs = regs st 
  , mem = mem st
  , tapes = tapes st
  , flag = flag st
}

to_bool :: Wrd -> Maybe Bool
to_bool 0 = Just False
to_bool 1 = Just True
to_bool _ = Nothing

get_pair::Bool -> (a,a) -> a
get_pair False = fst
get_pair True = snd

set_pair::Bool -> a -> (a,a) -> (a,a)
set_pair False a (_, b)= (a, b)
set_pair True b (a, _) = (a,b)

get_tape::State -> Bool -> Tape
get_tape st b = get_pair b (tapes st)

pop_tape::Tape -> Maybe (Wrd, Tape)
pop_tape (x:tp) = Just ( x,tp)
pop_tape _ = Nothing

set_tape::State -> Maybe Bool -> Reg -> Maybe (Wrd, Tape) -> State
set_tape st (Just tn) r (Just (x, tp)) =  State {
  pc = pc st
  , regs = update r x (regs st)
  , mem = mem st
  , tapes = set_pair tn tp (tapes st)
  , flag = flag st
}
set_tape st _ r _ = set_reg st r 0
 


next:: State -> State
next st = set_pc st (succ $ pc st )

-- # Interpreter

-- ## Utility evaluators

-- ### unary and binart operations

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

-- ### Conditionals

exec_cnd :: State -> Reg -> Either Reg Wrd -> ( Wrd -> Wrd -> Bool) -> State
exec_cnd st r1 a f = next $ set_flag st (bop (regs st) r1 a f)

-- ###Jump

exec_jmp st a = set_pc st (get_either (regs st) a)

-- ## Instruction execution (after instr. fetching)

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
exec (Iload r1 a) st = next $ set_reg st r1 (mem st (get_either (regs st) a))
exec (Iread r1 a) st = next $ let tn = (to_bool (get_either (regs st) a)) in
                                set_tape st tn r1 (pop_tape =<< (get_tape st <$> tn))


-- ## Program step
type Prog = Program Reg Wrd

step :: Prog -> State -> State
step prog st = exec (prog !! pc st) st


-- ## Execution
type Trace = [State]
run ::  Int -> Tape -> Tape -> Prog -> Trace
run k x w prog = (init_state k x w):(map (step prog) $ run k x w prog)

