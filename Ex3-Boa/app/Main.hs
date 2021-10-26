module Main where

import qualified Data.Text.IO as T
import Options.Applicative

import Language.Boa

--import qualified Data.Text.IO as T
--import System.IO (stderr)

data Opts = Opts
  { _optsFileName :: String
  , _optsCommand :: CompilerOperation
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  args <- execParser opts
  let fileName = _optsFileName args
  fileContents <- T.readFile fileName
  let inputFile = InputFile fileName fileContents
  compile inputFile $ _optsCommand args



opts :: ParserInfo Opts
opts = info (helper <*> options) fullDesc

options :: Parser Opts
options = Opts <$> (argument str (metavar "INPUT FILE")) <*> pCommand

pCommand :: Parser CompilerOperation
pCommand = pDumpParsedAst <|> pDumpAsm <|> pCompileToObjectFile
  where
    pDumpParsedAst = flag' DumpParsedAst
      (  long "dump-ast"
      <> help "Prints the parsed AST for a given program" )
    pDumpAsm = flag' DumpAsm
      (  long "dump-asm"
      <> help "Prints the generated assembly code for a given program" )
    pCompileToObjectFile = flag' CompileToObjectFile
      (  long  "compile"
      <> help "compiles the given program into an object file" )
