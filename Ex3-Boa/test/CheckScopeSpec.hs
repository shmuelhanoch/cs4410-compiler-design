{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for CheckScope.
module CheckScopeSpec where

import Data.Generics.Fixplate.Attributes (synthetise)
import Data.Generics.Fixplate.Base (Attr, Mu (Fix))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

import Language.Boa (position, mkPos, ExprF (EId, ELet, ENumber, EPrim1), Prim1 (Add1)i, checkScope)

ea :: Position
ea = mkPos "" (1, 1) (1, 1)

mkLet :: [(Text, Mu ExprF)] -> Mu ExprF -> Attr ExprF Position
mkLet bs e = synthetise (const ea) $ Fix $ ELet bs e

spec :: Spec
spec = do
  describe "positive cases: should do nothing" $ do
    it "let expr with a single binding" $
      checkScope (mkLet [("x", Fix $ ENumber 5)] (Fix $ EId "x"))
        `shouldBe` Right ()
    it "let expr with multiple bindings" $
      checkScope
        ( mkLet
            [ ("a", Fix $ ENumber 5)
            , ("b", Fix $ EPrim1 Add1 $ Fix $ EId "a")
            ]
            (Fix $ EId "x")
        )
        `shouldBe` Right ()
  describe "negative cases: duplicate bindings" $ do
    it "simple let expr with duplicate bindings" $
      checkScope
        ( mkLet
            [ ("a", Fix $ ENumber 5)
            , ("a", Fix $ ENumber 6)
            ]
            (Fix $ EId "x")
        )
        `shouldBe` Left "xxx"
