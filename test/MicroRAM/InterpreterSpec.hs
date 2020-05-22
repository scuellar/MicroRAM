module MicroRAM.InterpreterSpec where


import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck


import MicroRAM.MicroRAM
import MicroRAM.MRAMInterpreter
import Data.Sequence as Seq


main :: IO ()
main = defaultMain tests
  -- defaultMain (testGroup "Our Library Tests" testSuite) -- testSuit defined at eof
k = 5 -- 5 registers

-- We are treating the first register as the return
-- To get ouptu to get output provide a program and a number of steps to get the result after that long
-- Execute gets the trace and then looks at the first register after t steps
get_trace prog input advice = run k input advice prog
exec prog input advice steps = Seq.index (see (get_trace prog input advice) steps) 0 -- this throws an error if there is no register 0
simpl_exec prog steps = exec prog [] [] steps -- when there are no inputs

-- The tester setup

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

run' = run k [] []
prog1 :: Prog
prog1 = [Iadd 0 0 (Right 1),
       Iadd 0 0 (Right 2),
       Iadd 1 1 (Right 3),
       Iadd 1 1 (Right 4),
       Imull 0 0 (Left 1)]

run1 = run' prog1

test1 :: TestTree
test1 = testProperty "Testing (1+2)*(3+4) == 21" $ (simpl_exec prog1 5) == 21



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
test2_results_list = map ((Seq.lookup 0) . regs) (Prelude.take 11 run2) == map Just [0,1,1,2,2,3,3,4,4,5,5] 
test2 = testProperty "Test `x++` on a loop" $ \n -> (n :: Int) >= 0 ==> simpl_exec prog2 (2*n) == n

-- # Test 3: fibonacci
{-
   x = 1
   while true {
     z = x
     x = x + y
     y = z
   } 
-}

fib_pure :: Int -> Int
fib_pure 0 = 0
fib_pure 1 = 1
fib_pure n = fib_pure (n-1) + fib_pure (n-2) 

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

claimEqual :: (Eq a, Show a) => a -> a -> Either String String
claimEqual a b = 
   if a == b
     then Right "OK"
     else Left $ "Got " ++ show a ++ " but expected " ++ show b

test3 = testProperty "Test fibonacci" $ \n -> (n :: Int) >= 0 ==>
   claimEqual (simpl_exec prog3 (1+4*n)) ((fib_pure (n+1)))
   
-- # Test 4: conditional + input

run4:: Int -> Trace
run4 input = run k [input] [] prog4
prog4 :: Prog
prog4 = [Iread 1 (Right 0), --
         Icmpg 1 (Right 10), -- 1
         Icjmp (Right 5),    -- 2 
         Iadd 0 0 (Right 77),-- 3
         Ijmp (Right 6),     -- 4
         Iadd 0 0 (Right 42), -- Label: 5
         Ijmp (Right 6) -- Label: 6
        ]

test4 = testProperty "Test a conditional and input" $ \x ->
   claimEqual (exec prog4 [(x::Int)] [] 5) (if x>10 then 42 else 77)

                                                               
-- # Test 5: sum all input
  {- for i in input
        x =+ i
-}


run5:: [Int] -> Trace
run5 input = run k input [] prog5
prog5 :: Prog
prog5 = [Iread 1 (Right 0), --
         Iadd 0 0 (Left 1), -- 1
         Icjmp (Right 4),   -- 2
         Ijmp (Right 0),    -- 3
         Ijmp (Right 4)     -- 4
        ]

--test5 ls = Seq.lookup 0 (see (run5 ls) (4* (Prelude.length ls))) == (Just $ sum ls)
test5 = testProperty "Test adding a list of inputs" $ \xs ->
   claimEqual (exec prog5 (xs::[Int]) [] (4* (Prelude.length xs))) (sum xs)

tests = testGroup "Testing the Interpreter for  MicroRAM" [test1,test2,test3,test4,test5]
