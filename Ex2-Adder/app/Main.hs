module Main where

import Compile

import qualified Data.Text.IO as T
import System.IO (stderr)

main :: IO ()
main = T.getContents >>= either (T.hPutStrLn stderr) T.putStrLn . compile
