module TestInterpreter
    () where

import MicroRAM
import MRAMInterpreter
import Data.Sequence as Seq

k = 5

-- The tester

get_regs :: State -> Regs
get_regs = regs

see:: Trace -> Int -> Regs
see t n = regs (t !! n)

reg_trace::Trace -> [Regs]
reg_trace t= map regs t

pc_trace::Trace -> [Int]
pc_trace t= map pc t

flag_trace::Trace -> [Bool]
flag_trace t= map flag t

-- # Test 1
{- (1+2)*(3+4) = 21
-}

input = []
advice = []
run' = run k input advice
prog :: Prog
prog = [Iadd 0 0 (Right 1),
       Iadd 0 0 (Right 2),
       Iadd 1 1 (Right 3),
       Iadd 1 1 (Right 4),
       Imull 0 0 (Left 1)]

run1 = run' prog
test1 = Seq.lookup 0 (see run1 5) == Just 21


-- # Test 2
{-
   while true {
     x++
   } 
-}

prog2 :: Prog
prog2 = [Iadd 0 0 (Right 1),
       Ijmp (Right 0)]

run2 = run' prog2
test2 = map ((Seq.lookup 0) . regs) (Prelude.take 11 run2) == map Just [0,1,1,2,2,3,3,4,4,5,5] 


-- # Test 3: fibonacci
{-
   x = 1
   while true {
     z = x
     x = x + y
     y = z
   } 
-}

prog3 :: Prog
prog3 = [Iadd 0 1 (Right 1), -- x=1
         Iadd 2 0 (Right 0), -- z=x
         Iadd 0 0 (Left 1),  -- x=x+y
         Iadd 1 2 (Right 0),
       Ijmp (Right 1)]

run3 = run' prog3

fibs:: [Int]
fibs = 0 : 1 : Prelude.zipWith (+) fibs (tail fibs)
fib::Int -> Int
fib n = fibs !! n

test3 n= Seq.lookup 0 (see run3 (1+4*n)) == Just (fib (n+1))

-- # Test 4: conditional + input

run4:: Int -> Trace
run4 input = run k [input] advice prog4
prog4 :: Prog
prog4 = [Iread 1 (Right 0), --
         Icmpg 1 (Right 10), -- 1
         Icjmp (Right 5),    -- 2 
         Iadd 0 0 (Right 77),-- 3
         Ijmp (Right 6),     -- 4
         Iadd 0 0 (Right 42), -- Label: 5
         Ijmp (Right 6) -- Label: 6
        ]

test4 n = Seq.index (see (run4 n) 5) 0 == if n>10 then 42 else 77
                        
-- # Test 5: sum all input
  {- for i in input
        x =+ i
-}


run5:: [Int] -> Trace
run5 input = run k input advice prog5
prog5 :: Prog
prog5 = [Iread 1 (Right 0), --
         Iadd 0 0 (Left 1), -- 1
         Icjmp (Right 4),   -- 2
         Ijmp (Right 0),    -- 3
         Ijmp (Right 4)     -- 4
        ]

test5 ls = Seq.lookup 0 (see (run5 ls) (4* (Prelude.length ls))) == (Just $ sum ls)

