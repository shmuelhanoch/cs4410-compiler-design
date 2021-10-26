-- |

module TagSpec where

import Data.Generics.Fixplate.Base (Ann (Ann), Mu (Fix))
import qualified Data.Text as T (unlines)
import Test.Hspec (Spec, describe, it, shouldBe)

import Languague.Boa
  ( ExprF (EId, ELet, ENumber, EPrim1, EPrim2),
    Prim1 (Add1, Sub1),
    Prim2 (Add),
    mkPos,
    parseExpr,
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
