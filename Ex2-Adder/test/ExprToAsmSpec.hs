{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the CodeGen module

module ExprToAsmSpec where

import qualified Data.Text as T (intercalate)

import Test.Hspec
import ExprToAsm
import Syntax

spec :: Spec
spec = do
  describe "compileExpr on arithmetic expressions" $ do
    it "Compiles a plain number" $
      compileExpr (ENumber () 666) `shouldBe` Right "mov rax, 666"
    it "Compiles an add expression" $
      compileExpr (EPrim1 () PAdd1 (ENumber () 666))
      `shouldBe` Right
       ( T.intercalate "\n"
         [ "mov rax, 666"
         , "add rax, 1"
         ]
       )
    it "Compiles a nested arithmetic expression" $
      compileExpr (EPrim1 () PAdd1 (EPrim1 () PSub1 (ENumber () 666)))
      `shouldBe` Right
       ( T.intercalate "\n"
         [ "mov rax, 666"
         , "add rax, -1"
         , "add rax, 1"
         ]
       )
