
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module: PPAsm

module Language.Boa.PPAsm (asmToStr) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import Language.Boa.Asm


asmToStr :: [Instruction] -> Text
asmToStr = T.intercalate "\n" . map ppInstruction

ppInstruction :: Instruction -> Text
ppInstruction = T.pack . \case
  IMov lhs rhs -> printf
    "mov %s, %s" (ppArg lhs) (ppArg rhs)
  IAdd lhs rhs -> printf
    "add %s, %s" (ppArg lhs) (ppArg rhs)
  ISub lhs rhs -> printf
    "sub %s, %s" (ppArg lhs) (ppArg rhs)
  IMul lhs rhs -> printf
    "mul %s, %s" (ppArg lhs) (ppArg rhs)
  ILabel label -> printf
    "%s:" label
  ICmp lhs rhs -> printf
    "cmp %s, %s" (ppArg lhs) (ppArg rhs)
  IJne addr -> printf
    "jne %s" addr
  IJe addr -> printf
    "je %s" addr
  IJmp addr -> printf
    "jmp %s" addr

ppArg :: Arg -> Text
ppArg = \case
  ALit n -> T.pack $ show n
  AReg reg -> ppReg reg
  ARegOffset reg n ->
    if n < 0
      then T.pack $ printf "[%s%d]" (ppReg reg) (n * 8)
      else T.pack $ printf "[%s+%d]" (ppReg reg) (n * 8)

ppReg :: Reg -> Text
ppReg = \case
  RAX -> "rax"
  RSP -> "rsp"
