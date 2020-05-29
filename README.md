# MicroRAM
 
 This is a toy experiment containing:
 
 * An implementation of TinyRAM 
 * A interpreter of TinyRAM in Haskell 
 * A compiler from LLVM to TinyRAM


There is a collection of examples in the `examples` folder. You can run them like so

```
% stack build
% stack exec MicroRAM-exe examples/fibonacci 4
\\ Prints trace with 40 steps of running the program
\\ Register 0 should contain the 10th fibonacci
```