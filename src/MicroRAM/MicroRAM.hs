{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module MicroRAM.MicroRAM
( Instruction'(..),
  MAInstruction,
  Instruction,
  NamedBlock(NBlock),
  MAProgram,
  Program,
  Operand'(..),
  Operand,
  MAOperand) where

{-

From TinyRAM paper:

+---------+----------+----------------------------------------------+--------------+
|  Instr  | operands |                   effects                    |     flag     |
+---------+----------+----------------------------------------------+--------------+
| and     | ri rj A  | bitwise AND of [rj] and [A] and store in ri  | result is 0W |
| or      | ri rj A  | bitwise OR of [rj] and [A] and store in ri   | result is 0W |
| xor     | ri rj A  | bitwise XOR of [rj] and [A] and store in ri  | result is 0W |
| not     | ri A     | bitwise NOT of [A] and store result in ri    | result is 0W |
| add     | ri rj A  | [rj]u + [A]u and store result in ri          | overflow     |
| sub     | ri rj A  | [rj]u − [A]u and store result in ri          | borrow       |
| mull    | ri rj A  | [rj]u × [A]u, store least sign. bits in ri   | overflow     |
| umulh   | ri rj A  | [rj]u × [A]u, store most sign. bits in ri    | overflow     |
| smulh   | ri rj A  | [rj]s × [A]s, store most sign. bits in ri    | over/underf. |
| udiv    | ri rj A  | quotient of [rj]u/[A]u and store in ri       | [A]u = 0     |
| umod    | ri rj A  | remainder of [rj]u/[A]u and store in ri      | [A]u = 0     |
| shl     | ri rj A  | shift [rj] by [A]u bits left, store in ri    | MSB of [rj]  |
| shr     | ri rj A  | shift [rj] by [A]u bits right, store in ri   | LSB of [rj]  |
+---------+----------+----------------------------------------------+--------------+
| cmpe    | ri A     | none (“compare equal”)                       | [ri] = [A]   |
| cmpa    | ri A     | none (“compare above”, unsigned)             | [ri]u > [A]u |
| cmpae   | ri A     | none (“compare above or equal”, unsigned)    | [ri]u ≥ [A]u |
| cmpg    | ri A     | none (“compare greater”, signed)             | [ri]s > [A]s |
| cmpge   | ri A     | none (“compare greater or equal”, signed)    | [ri]s ≥ [A]s |
+---------+----------+----------------------------------------------+--------------+
| mov     | ri A     | store [A] in ri                              |              |
| cmov    | ri A     | if flag = 1, store [A] in ri                 |              |
+---------+----------+----------------------------------------------+--------------+
| jmp     | A        | set pc to [A]                                |              |
| cjmp    | A        | if flag = 1, set pc to [A] (else pc++)       |              |
| cnjmp   | A        | if flag = 0, set pc to [A] (else pc++)       |              |
+---------+----------+----------------------------------------------+--------------+
| store   | A ri     | store [ri] at memory address [A]u            |              |
| load    | ri A     | store content of mem address [A]u in ri      |              |
| read    | ri A     | if [A]u-th tape has words, consume next      |              |
|         |          | word, store in ri and set flag = 0;          | <-- (1)      |
|         |          | else store 0W in ri and set flag = 1         |              |
| answer  | A        | stall or halt (ret. value is [A]u)           | (2)          |
+---------+----------+----------------------------------------------+--------------+
| (read)   All but the first two tapes are empty: if [A]u 6∈ {0, 1} then store 0W  |
|          in ri and set flag = 1.                                                 |
+----------------------------------------------------------------------------------+
| (answer) answer causes a stall (i.e., not increment pc) or a halt                |
|          (i.e., the computation stops); the choice between the two is undefined. |
+----------------------------------------------------------------------------------+

The language defined bellow is 2 different languages:
 - MicroASM:
   Which represents the TinyRAM assembly language (Similar to assembly desccribed in TinyRAM paper)
 - MicroRAM
   Which is my implementation of TinyRAM
The only difference is that MicroASM can use as operands. That is intended to be a stand in
before assembly... but I also use it to stand in for label addresses before we know what they are.



-}

import GHC.Read
import Text.Read.Lex
import Text.ParserCombinators.ReadPrec

-- ## Registers (see assumptions above)
{-   regNum >= 8 
     + 0. Accumulator register (AX). Used in arithmetic operations
     + 1. Counter register (CX). Used in shift/rotate instructions and loops.
     + 2. Data register (DX). Used in arithmetic operations and I/O operations.
     + 3. Base register (BX). Used as a pointer to data (located in segment register DS, when in segmented mode).
     + 4. Stack Pointer register (SP). Pointer to the top of the stack.
     + 5. Stack Base Pointer register (BP). Used to point to the base of the stack.
     + 6. Source Index register (SI). Used as a pointer to a source in stream operations.
     + 7. Destination Index register (DI). Used as a pointer to a destination in stream operations.
-}
ax = 0::Int
cx = 1::Int
dx = 2::Int
bx = 3::Int
sp = 4::Int
bp = 5::Int
si = 6::Int
di = 7::Int

{-
data Phase = Pre | Post
  deriving (Eq, Ord, Read, Show)

-- | Operands
-- TinyRAM instructions take immidiate values (constants) and registers
-- when an instruction allows either we denote it a (|A| in the paper).
data Operand phase regT wrdT where
  Reg :: regT -> Operand phase regT wrdT
  Const :: wrdT -> Operand phase regT wrdT
  Label :: String -> Operand Pre regT wrdT
  HereLabel :: Operand Pre regT wrdT

-}
-- | Phase: This language can be instantiated at different levels:
-- Pre: Indicates MicroAssembly where the program is made of
-- labeled blocks and Operands can be labels
-- Post: indicates Pure MicroRAM where the program is a list of instructions
-- at this level, labels have been removed.
data Phase = Pre | Post
  deriving (Eq, Ord, Read, Show)

-- | Operands
-- TinyRAM instructions take immidiate values (constants) and registers
-- when an instruction allows either we denote it a (|A| in the paper).
data Operand' phase regT wrdT where
  Reg :: regT -> Operand' phase regT wrdT
  Const :: wrdT -> Operand' phase regT wrdT
  Label :: String -> Operand' Pre regT wrdT
  HereLabel :: Operand' Pre regT wrdT
  
deriving instance (Eq regT, Eq wrdT) => Eq (Operand' phase regT wrdT)
deriving instance (Ord regT, Ord wrdT) => Ord (Operand' phase regT wrdT)
deriving instance (Read regT, Read wrdT) => Read (Operand' Pre regT wrdT)
deriving instance (Show regT, Show wrdT) => Show (Operand' phase regT wrdT)

-- | Reading Operands
-- Generated using --ddump-derived 
instance (Read regT, Read wrdT) => Read (Operand' Post regT wrdT) where
    readPrec
      = parens (do expectP (Ident "Reg")
                   a1_a1rK <- step readPrec
                   return (MicroRAM.MicroRAM.Reg a1_a1rK))
        +++
        (prec 10
         (do expectP (Ident "Const")
             a1_a1rL <- step readPrec
             return (MicroRAM.MicroRAM.Const a1_a1rL)))
    readList = GHC.Read.readListDefault
    readListPrec = GHC.Read.readListPrecDefault


-- | TinyRAM Instructions
data Instruction' regT operand =
  -- Bit Operations
  Iand regT regT operand     --compute bitwise AND of [rj] and [A] and store result in ri
  | Ior regT regT operand    --compute bitwise OR of [rj] and [A] and store result in ri
  | Ixor regT regT operand   --compute bitwise XOR of [rj] and [A] and store result in ri
  | Inot regT operand        --compute bitwise NOT of [A] and store result in ri
  -- Integer Operations
  | Iadd regT regT operand   --compute [rj]u + [A]u and store result in ri
  | Isub regT regT operand   --compute [rj]u − [A]u and store result in ri
  | Imull regT regT operand  --compute [rj]u × [A]u and store least significant bits of result in ri
  | Iumulh regT regT operand --compute [rj]u × [A]u and store most significant bits of result in ri
  | Ismulh regT regT operand --compute [rj]s × [A]s and store most significant bits of result in ri 
  | Iudiv regT regT operand  --compute quotient of [rj ]u /[A]u and store result in ri
  | Iumod regT regT operand  --compute remainder of [rj ]u /[A]u and store result in ri
  -- Shift operations
  | Ishl regT regT operand   --shift [rj] by [A]u bits to the left and store result in ri
  | Ishr regT regT operand   --shift [rj] by [A]u bits to the right and store result in ri
  -- Compare Operations
  | Icmpe regT operand       --none (“compare equal”)
  | Icmpa regT operand       --none (“compare above”, unsigned)
  | Icmpae regT operand      --none (“compare above or equal”, unsigned)
  | Icmpg regT operand       --none (“compare greater”, signed)
  | Icmpge regT operand      --none (“compare greater or equal”, signed)
  -- Move operations
  | Imov regT operand        -- store [A] in ri
  | Icmov regT operand       -- iff lag=1, store [A] in ri
  -- Jump operations
  | Ijmp operand             -- set pc to [A]
  | Icjmp operand            -- if flag = 1, set pc to [A] (else increment pc as usual)
  | Icnjmp operand           -- if flag = 0, set pc to [A] (else increment pc as usual)
  -- Memory operations
  | Istore operand regT      -- store [ri] at memory address [A]u
  | Iload regT operand       -- store the content of memory address [A]u into ri 
  | Iread regT operand       -- if the [A]u-th tape has remaining words then consume the next word,
                                       -- store it in ri, and set flag = 0; otherwise store 0W in ri and set flag = 1
  | Ianswer operand          -- stall or halt (and the return value is [A]u)
  deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- ** MicroAssembly
type MAOperand regT wrdT = Operand' Pre regT wrdT
type MAInstruction regT wrdT = Instruction' regT (MAOperand regT wrdT)

data NamedBlock r w = NBlock (Maybe String) [MAInstruction r w]
  deriving (Eq, Ord, Read, Show)
type MAProgram r w = [NamedBlock r w] -- These are MicroASM programs


-- ** MicroRAM
type Operand regT wrdT = Operand' Post regT wrdT
type Instruction regT wrdT = Instruction' regT (Operand regT wrdT)
type Program r w = [Instruction r w]
