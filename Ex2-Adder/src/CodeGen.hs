module CodeGen (compileProg) where

import Syntax

import Data.Map (Map)
import Data.Text (Text)

data Reg
  = RAX -- used for return values
  | RSP -- holds the current stack pointer

data InstArg
  = IALit Int
  | IAReg Reg
  | IARegOffset Reg Int

data Instruction
 = IMov InstArg InstArg

type Env = Map Text Int

compileExpr :: Expr a -> [Instruction]
compileExpr = undefined

compileProg :: Expr a -> Either Text Text
compileProg = undefined
