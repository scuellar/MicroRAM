{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeOperators #-}


{-|
Module      : MicrRAMMAp and Fold
Description : Generic functionallity to map over an MicroRAM program
Maintainer  : santiago@galois.com
Stability   : experimental

Like Map for list, but for MicroRAM programs.

It takes a Mapper how to map each part of the program.

Everything is parametric over a generic environment env that is passed around. But
this is not an acumulator and cannot be modified maybe I'll ad a Fold


-}

module Compiler.MRAMMap
    ( Mapper,
      --Folder,
      --mramMap
      --mramFold
    ) where

import MicroRAM.MicroRAM 


type ErrMonad = Either String

-- | Mapper: the functions to map over the program. 
data Mapper acc regT wrdT = Mapper {
  initAcc :: acc -- ^accumulator
  , accumulate :: acc -> Instruction regT wrdT -> acc
  , regMap :: acc -> regT -> ErrMonad regT 
  , operMap :: acc -> Operand regT wrdT -> ErrMonad (Operand regT wrdT)
  }

mapRegOp :: Mapper acc regT wrdT -> acc ->Instruction regT wrdT -> ErrMonad (Instruction regT wrdT)
mapRegOp mpr acc (Iand r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Iand r1' r2' a'
mapRegOp mpr acc (Ior r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Ior r1' r2' a'
mapRegOp mpr acc (Ixor r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Ixor r1' r2' a'
mapRegOp mpr acc (Inot r1 a) = do
  r1' <- regMap mpr acc  r1
  a' <- operMap mpr acc a
  Right $ Inot r1' a'
mapRegOp mpr acc (Iadd r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Iadd r1' r2' a'
mapRegOp mpr acc (Isub r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Isub r1' r2' a'
mapRegOp mpr acc (Imull r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Imull r1' r2' a'
mapRegOp mpr acc (Iumulh r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Iumulh r1' r2' a'
mapRegOp mpr acc (Ismulh r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Ismulh r1' r2' a'
mapRegOp mpr acc (Iudiv r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Iudiv r1' r2' a'
mapRegOp mpr acc (Iumod r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Iumod r1' r2' a'
mapRegOp mpr acc (Ishl r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Ishl r1' r2' a'
mapRegOp mpr acc (Ishr r1 r2 a) = do
  r1' <- regMap mpr acc  r1  
  r2' <- regMap mpr acc  r2  
  a' <- operMap mpr acc a
  Right $ Ishr r1' r2' a'
-- --------------------
mapRegOp mpr acc (Icmpe r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Icmpe r1' a'
mapRegOp mpr acc (Icmpa r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Icmpa r1' a'
mapRegOp mpr acc (Icmpae r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Icmpae r1' a'
mapRegOp mpr acc (Icmpg r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Icmpg r1' a'
mapRegOp mpr acc (Icmpge r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Icmpge r1' a'
-- --------------------
mapRegOp mpr acc (Imov r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Imov r1' a'
mapRegOp mpr acc (Icmov r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Icmov r1' a'
-- --------------------
mapRegOp mpr acc (Ijmp a) = do  
  a' <- operMap mpr acc a
  Right $ Ijmp a'
mapRegOp mpr acc (Icjmp a) = do  
  a' <- operMap mpr acc a
  Right $ Icjmp a'
mapRegOp mpr acc (Icnjmp a) = do  
  a' <- operMap mpr acc a
  Right $ Icnjmp a'
-- --------------------
mapRegOp mpr acc (Istore a r1) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Istore a' r1'
mapRegOp mpr acc (Iload r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Iload r1' a'
mapRegOp mpr acc (Iread r1 a) = do
  r1' <- regMap mpr acc  r1  
  a' <- operMap mpr acc a
  Right $ Iread r1' a'
mapRegOp mpr acc (Ianswer a) = do  
  a' <- operMap mpr acc a
  Right $ Ianswer a'


{-
 mapInstr ::
  Mapper acc regT wrdT ->
  acc -> Instruction regT wrdT ->
  ErrMonad [Instruction regT wrdT]
mapInstr mpr a inst = do
  instr' <- mapRegOp mpr a inst
  instrMap mpr a instr'
  

mramMap :: Mapper acc regT wrdT -> a -> [Instruction regT wrdT] -> ErrMonad [Instruction regT wrdT]
mramMap mpr prog = do
  onePass <- mapM (mapInstr mpr) prog
  Right (concat onePass) 
-}
