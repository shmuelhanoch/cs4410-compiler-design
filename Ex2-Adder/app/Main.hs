{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compile

import qualified Data.Text.IO as T

main :: IO ()
main = either T.putStrLn T.putStrLn $ compile ""
