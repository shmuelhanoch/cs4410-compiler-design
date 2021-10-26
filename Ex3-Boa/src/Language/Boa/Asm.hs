module Language.Boa.Asm where

import Data.Text (Text)

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
 | ISub Arg Arg
 | IMul Arg Arg
 | ILabel Text
 | ICmp Arg Arg
 | IJne Text
 | IJe Text
 | IJmp Text
