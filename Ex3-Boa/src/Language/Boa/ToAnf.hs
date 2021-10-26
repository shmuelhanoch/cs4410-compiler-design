
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module Language.Boa.contains the logic of coverting an expression into an A-Normal form.
-- Am expression is in A-Nornal form, or ANF, if each operator's arguemnts are immediate
-- terms

module Language.Boa.ToAnf (isAnf, toAnf) where

import Control.Lens (_2)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Generics.Fixplate (Ann(Ann, unAnn), Attr, Mu (Fix, unFix), attribute)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Printf (printf)

import Language.Boa.Syntax (ExprF (..))
import Language.Boa.Tag (Tag(..))

newtype AnfBind e = AnfBind [(Text, e)]
  deriving (Semigroup, Monoid)

newBinding :: Attr ExprF Tag -> Writer (AnfBind (Attr ExprF ())) Text
newBinding expr = do
  let freshId = tagToId $ unTag $ attribute expr
  tell $ AnfBind [(freshId, removeAnn expr)]
  pure freshId
  where
    tagToId tg = T.pack $ printf "__Anf_Temp_%d" tg
    removeAnn ex = Fix $ Ann () $ fmap removeAnn $ unAnn $  unFix ex

isAnf :: Attr ExprF () -> Bool
isAnf = \case
  Fix (Ann _ (ELet bs e)) -> all (isAnf . snd) bs && isAnf e
  Fix (Ann _ (EPrim1 _ e)) -> isImm e
  Fix (Ann _ (EPrim2 _ lhs rhs)) -> isImm lhs && isImm rhs
  Fix (Ann _ (EIf cond thn els)) -> isImm cond && isAnf thn && isAnf els
  Fix (Ann _ (ENumber _)) -> True
  Fix (Ann _ (EId _)) -> True
  where
    isImm = \case
      Fix (Ann _ (ENumber _)) -> True
      Fix (Ann _ (EId _)) -> True
      _ -> False


toAnf :: Attr ExprF Tag -> Attr ExprF ()
toAnf expr =
  let (expr', AnfBind binds) = runWriter $ toAnf' expr
  in Fix $ Ann () $ ELet binds expr'

toAnf' :: Attr ExprF Tag -> Writer (AnfBind (Attr ExprF ())) (Attr ExprF ())
toAnf' = \case
  Fix (Ann _ (ELet bs e)) -> do
    e' <- ELet <$> traverse (_2 toAnf') bs <*> toAnf' e
    pure $ Fix $ Ann () e'
  Fix (Ann _ (EPrim1 op e)) -> do
    newId <- newBinding e
    pure $ Fix $ Ann () $ EPrim1 op $ Fix $ Ann () $ EId newId
  Fix (Ann _ (EPrim2 op lhs rhs)) -> do
    lhsId <- newBinding lhs
    rhsId <- newBinding rhs
    pure $ Fix $ Ann () $ EPrim2 op (Fix $ Ann () $ EId lhsId) (Fix $ Ann () $ EId rhsId)
  Fix (Ann _ (EIf cond thn els)) -> do
    e' <- EIf <$> toAnf' cond <*> toAnf' thn <*> toAnf' els
    pure $ Fix $ Ann () e'
  Fix (Ann _ (ENumber n)) -> pure $ Fix $ Ann () $ ENumber n
  Fix (Ann _ (EId nm)) -> pure $ Fix $ Ann () $ EId nm
