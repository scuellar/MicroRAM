{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : Assembler
Description : MicroASM -> MicroRAM
Maintainer  : santiago@galois.com
Stability   : experimental

This module compiles Translates MicroASM to MicroRAM.

MicroASM is different to MicrRAM in that it allows the operands
`Label` and `HereLabel`. The assembler will replace those labels
with the actual instruction numbers to obtain MicroRAM. In particular
a MicroASM program can be "partial" and needs to be linked to another part
that contains some of the labels (this allows some simple separta compilation.

The assembler translates all `Label` and `HereLabel` to the actual
instruction number to produce well formed MicroRAM. It does so in three passes:
 
1) Create a label map, mapping names -> instruction

2) "Flatten": Removing the names from blocks, leaving a list of instructions

3) Replace all labels with the location given in the label map.

TODO: It can all be done in 2 passes. Optimize?

-}

module Compiler.Assembler
    (assemble) where

{-
import qualified LLVM.AST as LLVM 
import qualified LLVM.AST.Constant as LLVM.Constant
import Control.Monad.State.Lazy
import Control.Monad.Except
import qualified Data.List as List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.Sequence as Seq (lookup, fromList)
import qualified Data.Word as Word
-}

import MicroRAM.MicroRAM
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import Data.Traversable
import Compiler.CodeGenerator (CgMonad,CgError(..)) -- For errors, this should be moved to a standalone.
-- * Assembler

type Wrd = Word

naturals :: [Wrd]
naturals = iterate (1 +) 1


-- ** Create a label map

type LabelMap = Map.Map String Wrd
getLabel :: LabelMap -> String -> CgMonad Wrd
getLabel lmap lbl =
  case Map.lookup  lbl lmap of
    Just w -> Right w
    Nothing -> Left $ CompilerAssumption "Code label called but not defined"

data State = State { pc :: Wrd, lMap :: LabelMap}
initState = State 0 Map.empty

-- | createMapStep
-- Updates the state given one block 
createMapStep :: State -> NamedBlock regT wrdT -> State
createMapStep st (NBlock name code) =
  State ( pc st + (fromIntegral $ length code)) (update oldMap name)
  where location = pc st
        oldMap = lMap st
        update lm (Just nm) = Map.insert nm location lm
        update lm _ = lm

createMap :: MAProgram regT wrdT -> LabelMap
createMap prog = lMap $ foldl createMapStep initState prog

-- ** Flaten the program
-- Now that the labels have been computed we can remove the labes
-- to get a "flat" list of (MicroAssembly) instructions 

flattenStep ::
  [MAInstruction regT wrdT]
  -> NamedBlock regT wrdT
  -> [MAInstruction regT wrdT]
flattenStep prog (NBlock _ code) = prog ++ code
  
flatten :: MAProgram regT wrdT -> [MAInstruction regT wrdT]
flatten maProg = foldl flattenStep [] maProg

-- ** Translate label
-- Given the label map, we can replace all labels
-- with the correct code location

-- | Translate the operands
-- This takes the current instruction to translate 'HereLabel'
translateOperand :: LabelMap -> Wrd -> MAOperand regT Wrd -> CgMonad (Operand regT Wrd)
translateOperand _ _ (Reg r) = Right $ Reg r
translateOperand _ _ (Const c) =  Right $ Const c
translateOperand lmap _ (Label lbl) = do
  location <- getLabel lmap lbl 
  Right $ Const location
translateOperand _ loc (HereLabel) = Right $ Const loc

translatePair:: Monad m =>
  (w -> a -> m b) ->
  (Instruction' regT a, w) ->
  m (Instruction' regT b)
translatePair f (inst,w) = mapM (f w) inst

translateProgram:: Monad m =>
  (w -> a -> m b) ->
  [(Instruction' regT a, w)] ->
  m [(Instruction' regT b)]
translateProgram f = mapM (translatePair f)

replaceLabels:: LabelMap -> [MAInstruction regT Wrd] -> CgMonad (Program regT Wrd)
replaceLabels lm amProg = translateProgram (translateOperand lm) numberedProg
  where numberedProg = zip amProg naturals 


-- ** Assemble : put all steps together

assemble :: MAProgram regT Wrd -> CgMonad (Program regT Wrd)
assemble massProg = replaceLabels lMap flatProg
  where lMap     = createMap massProg
        flatProg = flatten massProg
