module MicroRAM
( Instruction(..),
  Program) where


-- TinyRAM Instructions

data Instruction regT wrdT =
  -- Bit Operations
  Iand regT regT (Either regT wrdT)    --compute bitwise AND of [rj] and [A] and store result in ri
  | Ior regT regT (Either regT wrdT)   --compute bitwise OR of [rj] and [A] and store result in ri
  | Ixor regT regT (Either regT wrdT)  --compute bitwise XOR of [rj] and [A] and store result in ri
  | Inot regT (Either regT wrdT)       --compute bitwise NOT of [A] and store result in ri
  -- Integer Operations
  | Iadd regT regT (Either regT wrdT)  --compute [rj]u + [A]u and store result in ri
  | Isub regT regT (Either regT wrdT)  --compute [rj]u − [A]u and store result in ri
  | Imull regT regT (Either regT wrdT) --compute [rj]u × [A]u and store least significant bits of result in ri
  | Iumulh regT regT (Either regT wrdT)--compute [rj]u × [A]u and store most significant bits of result in ri
  | Ismulh regT regT (Either regT wrdT)--compute [rj]s × [A]s and store most significant bits of result in ri 
  | Iudiv regT regT (Either regT wrdT) --compute quotient of [rj ]u /[A]u and store result in ri
  | Iumod regT regT (Either regT wrdT)  --compute remainder of [rj ]u /[A]u and store result in ri
  -- Shift operations
  | Ishl regT regT (Either regT wrdT)  --shift [rj] by [A]u bits to the left and store result in ri
  | Ishr regT regT (Either regT wrdT)  --shift [rj] by [A]u bits to the right and store result in ri
  -- Compare Operations
  | Icmpe regT (Either regT wrdT)      --none (“compare equal”)
  | Icmpa regT (Either regT wrdT)      --none (“compare above”, unsigned)
  | Icmpae regT (Either regT wrdT)     --none (“compare above or equal”, unsigned)
  | Icmpg regT (Either regT wrdT)      --none (“compare greater”, signed)
  | Icmpge regT (Either regT wrdT)     --none (“compare greater or equal”, signed)
  -- Move operations
  | Imov regT (Either regT wrdT)       -- store [A] in ri
  | Icmov regT (Either regT wrdT)      -- iff lag=1, store [A] in ri
  -- Jump operations
  | Ijmp (Either regT wrdT)       -- set pc to [A]
  | Icjmp (Either regT wrdT)      -- if flag = 1, set pc to [A] (else increment pc as usual)
  | Icnjmp (Either regT wrdT)     -- if flag = 0, set pc to [A] (else increment pc as usual)
  -- Memory operations
  | Istore (Either regT wrdT) regT     -- store [ri] at memory address [A]u
  | Iload regT (Either regT wrdT)      -- store the content of memory address [A]u into ri 
  | Iread regT (Either regT wrdT)      -- if the [A]u-th tape has remaining words then consume the next word,
                                       -- store it in ri, and set flag = 0; otherwise store 0W in ri and set flag = 1
  | Ianswer (Either regT wrdT)         -- stall or halt (and the return value is [A]u)


-- ## The Program
type Program r w = [Instruction r w]
