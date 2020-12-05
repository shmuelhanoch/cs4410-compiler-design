{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the Parser module

module ParserSpec where

import Test.HSpec
import Parser

main :: IO ()
main = hspec $ do
  describe "parseExpr" $ do
    it "parses a plain number" $
      parseExpr "5" `shouldBe` Right ENumber (SourceLoc "" 1 1) 5
