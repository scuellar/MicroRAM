module Main where

import System.Environment

import Lib
import Compiler.Compiler
import MicroRAM.MicroRAM
import MicroRAM.MRAMInterpreter

prog1 = [Iadd 0 1 (Const 1), -- x=1
         Iadd 2 0 (Const 0), -- z=x
         Iadd 0 0 (Reg 1),  -- x=x+y
         Iadd 1 2 (Const 0),
       Ijmp (Const 1)]

-- Hardcode 16 registers for now
k = 16
-- No input or advice
input, advice :: Tape
input = []
advice = []

main :: IO ()
--main = print prog1

main = do
    args <- getArgs
    case args of 
      [file,steps] -> do
        x <- readFile file
        let prog = read x::(Program Int Word) in 
          let n = read steps::Int in 
            print (take n (run k input advice prog))  
      _ -> putStrLn "Wrong number of arguments"
