-- | This module is the entry point for the compiler.

module Compile where

--import Syntax
import Parser
import ExprToAsm

import Control.Monad
import Data.Text (Text)

-- | Takes a textual representation of a program in "Adder" language
-- and Returns the generated assembly code as @Text@.
-- To actually run the program, you have to use an assembler (e.g nasm) to create
-- an object file, and link it against the provided main.c stub.
compile :: Text -> Either Text Text
compile = parseExpr >=> compileProg
