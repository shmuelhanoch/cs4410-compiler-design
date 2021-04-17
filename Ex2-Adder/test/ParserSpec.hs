{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the Parser module

module ParserSpec (spec) where

import qualified Data.Text as T (unlines)
import Text.Megaparsec (SourcePos(..), mkPos)

import Test.Hspec
import Parser
import Syntax

p :: Int -> Int -> SourcePos
p x y = SourcePos "" (mkPos x) (mkPos y)

spec :: Spec
spec = do
  describe "parseExpr on arithmetic expressions" $ do
    it "parses a plain number" $
      parseExpr "5" `shouldBe` Right (ENumber (p 1 1) 5)
    it "parses a simple sub1 expression" $
      parseExpr "(sub1 5)" `shouldBe` Right (EPrim1 (p 1 1) PSub1 $ ENumber (p 1 7) 5)
    it "parses a nested expression" $
      parseExpr "(add1 (add1 (sub1 5)))"
        `shouldBe`
          Right (EPrim1 (p 1 1) PAdd1
            (EPrim1 (p 1 7) PAdd1
              (EPrim1 (p 1 13) PSub1
                (ENumber (p 1 19) 5))))
  describe "parseExpr with let bindings" $ do
    it "parses a simple let expression with one binding" $
      parseExpr "(let ((x 5)) (add1 x))"
      `shouldBe` Right
        (ELet (p 1 1)
         [ ("x", ENumber (p 1 10) 5)
         ] (EPrim1 (p 1 14) PAdd1 (EIden (p 1 20) "x")))
    it "parses a let expression with multiple bindings" $
      ( parseExpr $ T.unlines
          [ "(let ((x 5)"
          , "      (y (sub1 x)))"
          , "  (sub1 y))"
          ]
      ) `shouldBe` Right
          (ELet (p 1 1)
           [ ("x", ENumber (p 1 10) 5)
           , ("y", EPrim1 (p 2 10) PSub1 (EIden (p 2 16) "x"))
           ] (EPrim1 (p 3 3) PSub1 (EIden (p 3 9) "y")))
