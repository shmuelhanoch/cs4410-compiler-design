{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the Parser module
module ParserSpec (spec) where

import Data.Generics.Fixplate.Base (Ann (Ann), Mu (Fix))
import qualified Data.Text as T (unlines)
import Test.Hspec (Spec, describe, it, shouldBe)

import Language.Boa
  ( ExprF (EId, ELet, ENumber, EPrim1, EPrim2),
    Prim1 (Add1, Sub1),
    Prim2 (Add),
    mkPos,
    parser,
  )

spec :: Spec
spec = do
  describe "parsing arithmetic expressions" $ do
    it "parses a plain number" $
      parseExpr "5" `shouldBe` Right (Fix $ Ann (mkPos "" (1, 1) (1, 2)) (ENumber 5))
    it "parses a simple prefix expression" $
      parseExpr "sub1(5)"
        `shouldBe` Right
          (Fix $ Ann (mkPos "" (1, 1) (1, 8)) (EPrim1 Sub1 $ Fix $ Ann (mkPos "" (1, 6) (1, 7)) (ENumber 5)))
    it "parses a nested prefix expression" $
      parseExpr "add1(add1(sub1(5)))"
        `shouldBe` Right
          ( Fix $
              Ann
                (mkPos "" (1, 1) (1, 20))
                ( EPrim1
                    Add1
                    $ Fix $
                      Ann
                        (mkPos "" (1, 6) (1, 19))
                        ( EPrim1
                            Add1
                            $ Fix $
                              Ann
                                (mkPos "" (1, 11) (1, 18))
                                ( EPrim1
                                    Sub1
                                    $ Fix $
                                      Ann
                                        (mkPos "" (1, 16) (1, 17))
                                        (ENumber 5)
                                )
                        )
                )
          )

    it "parses a simple infix expression" $
      parseExpr "5 + 7"
        `shouldBe` Right
          ( Fix $
              Ann
                (mkPos "" (1, 1) (1, 6))
                ( EPrim2
                    Add
                    ( Fix $
                        Ann (mkPos "" (1, 1) (1, 2)) (ENumber 5)
                    )
                    ( Fix $
                        Ann (mkPos "" (1, 5) (1, 6)) (ENumber 7)
                    )
                )
          )

  describe "parsing let bindings" $ do
    it "parses a simple let expression with one binding" $
      parseExpr "let x = 5 in add1(x)"
        `shouldBe` Right
          ( Fix $
              Ann
                (mkPos "" (1, 1) (1, 21))
                ( ELet
                    [ ("x", Fix $ Ann (mkPos "" (1, 9) (1, 10)) (ENumber 5))
                    ]
                    ( Fix $
                        Ann
                          (mkPos "" (1, 14) (1, 21))
                          ( EPrim1 Add1 $
                              Fix $
                                Ann (mkPos "" (1, 19) (1, 20)) (EId "x")
                          )
                    )
                )
          )

    it "parses a let expression with multiple bindings" $
      parseExpr
        ( T.unlines
            [ "let x = 5,"
            , "    y = sub1(x)"
            , "in  sub1(y)"
            ]
        )
        `shouldBe` Right
          ( Fix $
              Ann
                (mkPos "" (1, 1) (4, 1))
                ( ELet
                    [ ("x", Fix $ Ann (mkPos "" (1, 9) (1, 10)) (ENumber 5))
                    ,
                      ( "y"
                      , Fix $
                          Ann
                            (mkPos "" (2, 9) (3, 1))
                            ( EPrim1 Sub1 $
                                Fix $
                                  Ann (mkPos "" (2, 14) (2, 15)) (EId "x")
                            )
                      )
                    ]
                    ( Fix $
                        Ann
                          (mkPos "" (3, 5) (4, 1))
                          ( EPrim1 Sub1 $
                              Fix $
                                Ann (mkPos "" (3, 10) (3, 11)) (EId "y")
                          )
                    )
                )
          )

  descirbes "parsing if expressions" $ do
    it "parses a simple if expression" $
      parseExpr "if 0: 5 else: 6"
        `shouldBe` Right
          ( Fix $
              Ann
                (mkPos "" (1, 1) (2, 1))
                ( EIf
                    (Fix $ Ann (mkPos "" (1, 4) (1, 5)) ENumber 0)
                    (Fix $ Ann (mkPos "" (1, 7) (1, 8)) ENumber 5)
                    (Fix $ Ann (mkPos "" (1, 15) (1, 16)) ENumber 6)
                )
          )
