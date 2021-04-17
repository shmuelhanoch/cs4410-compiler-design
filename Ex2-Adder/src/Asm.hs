
module Asm where

data Reg
  = RAX -- used for return values
  | RSP -- holds the current stack pointer

type Offset = Int

data Arg
  = ALit Int
  | AReg Reg
  | ARegOffset Reg Offset

data Instruction
 = IMov Arg Arg
 | IAdd Arg Arg
