
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module: PPAsm

module PPAsm (asmToStr) where

import Asm

import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)


asmToStr :: [Instruction] -> Text
asmToStr = T.intercalate "\n" . map ppInstruction

ppInstruction :: Instruction -> Text
ppInstruction = \case
  IMov lhs rhs -> T.pack $ printf
    "mov %s, %s" (ppArg lhs) (ppArg rhs)
  IAdd lhs rhs -> T.pack $ printf
    "add %s, %s" (ppArg lhs) (ppArg rhs)

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
