{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | This module Language.Boa.is the entry point for the compiler.
module Language.Boa.Compile where

import Control.Monad (void)
import Data.Text (Text)

import qualified Data.Text.IO as T
import System.IO (stderr)

import Language.Boa.CheckScope (checkScope)
import Language.Boa.Error ( reportDiagnostics)
import Language.Boa.ExprToAsm ( compileProg )
import Language.Boa.Parser ( parseExpr )
import Language.Boa.Rename (rename)
import Language.Boa.Tag (tag)
import Language.Boa.ToAnf (toAnf)

data CompilerOperation
  = DumpParsedAst
  | DumpAsm
  | CompileToObjectFile
  deriving (Eq, Show)

data InputFile = InputFile
  { _ctFile :: FilePath
  , _ctInput :: Text
  }
  deriving (Eq, Show)

-- | Takes a textual representation of a program in "Adder" language
-- and Returns the generated assembly code as @Text@.
-- To actually run the program, you have to use an assembler (e.g nasm) to create
-- an object file, and link it against the provided main.c stub.
compile :: InputFile -> CompilerOperation -> IO ()
compile file operation =
  let
    runOp = case operation of
      DumpParsedAst -> dumpParsedAst
      DumpAsm -> dumpAsm
      CompileToObjectFile -> compileToObjectFile
  in runOp file

dumpParsedAst :: InputFile -> IO ()
dumpParsedAst (InputFile file input) = either reportDiagnostics print
  $ parseExpr file input

dumpAsm :: InputFile -> IO ()
dumpAsm (InputFile file input) =
  let
    expr = do
        parsed <- parseExpr file input
        void $ checkScope parsed
        pure parsed
  in
    case expr of
      Left e -> reportDiagnostics e
      Right expr' ->
        let
          asm = do
            let taggedExpr = tag expr'
            renamedExpr <- rename taggedExpr
            let anfExpr = toAnf renamedExpr
            compileProg anfExpr
        in
          case asm of
          Left e -> internalError e
          Right asm' -> T.putStrLn asm'


compileToObjectFile :: InputFile -> IO ()
compileToObjectFile = undefined

internalError :: Text -> IO ()
internalError s = T.hPutStrLn stderr $ "Internal compiler error: " <> s
