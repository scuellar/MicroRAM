{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}

module RegAllocatorParams
    (codegenGlob
    ) where

import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified Data.List as List
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Short as Short
import qualified Data.Sequence as Seq (lookup, fromList)
import qualified Data.Word as Word

import qualified MicroRAM as MRAM
import qualified LLVM.AST.IntegerPredicate as IntPred

-- This parameter should be passed by the register allocator
-- Here I show one hackt instantiation that only works for at most (length ['a'..'z'])  
regNum :: Int
regNum = 16

allWords :: [[Char]]
allWords =  [""] ++ [a:b | b<-allWords, a<-['a'..'z']]  
regNames = take regNum allWords

string2int :: String -> Maybe Int
string2int = flip List.elemIndex regNames

name2register :: LLVM.Name -> Maybe Reg
name2register (LLVM.Name name) = string2int (C8.unpack (Short.fromShort name))
name2register _ = Nothing

register2string:: Reg -> Maybe String
register2string r = Seq.lookup r (Seq.fromList regNames) 
register2name :: Reg -> Maybe LLVM.Name
register2name r = LLVM.Name <$> Short.toShort <$> C8.pack <$> (register2string r)

