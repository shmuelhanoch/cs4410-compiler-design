{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | CheckScope is a rewrite which checks the AST for scoping error.
-- In particular, it checks for two things:
-- 1) Make sure that there are no duplicate duplicate bindings
-- 2) Make sure that each used variale is actually bounded.
module Language.Boa.CheckScope (checkScope) where

import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Generics.Fixplate.Base (Ann (Ann), Attr, Mu (Fix))
import qualified Data.Set as S
import Language.Boa.Error
import Language.Boa.SrcLoc (SrcSpan)
import Language.Boa.Syntax (ExprF (..))


checkScope :: Attr ExprF SrcSpan -> Either Messages ()
checkScope expr = checkDuplicateBindings expr *> checkUnboundVars expr

checkDuplicateBindings :: Attr ExprF SrcSpan -> Either Messages ()
checkDuplicateBindings = \case
  Fix (Ann ann (ELet bs _e))
    | hasDups bs -> Left $ singleMsg $ MsgEnvelope ann "Conflicting definitions"
  Fix e -> traverse_ checkScope e
  where
    hasDups (map fst -> xs) = ((/=) `on` length) (nubOrd xs) xs

checkUnboundVars :: Attr ExprF SrcSpan -> Either Messages ()
checkUnboundVars = go mempty
  where
    go boundVars = \case
      Fix (Ann _ (ELet bs e)) ->
        go (S.union (S.fromList $ map fst bs) boundVars) e
      Fix (Ann ann (EId nm))
        | nm `S.member` boundVars -> pure ()
        | otherwise -> Left $ singleMsg $ MsgEnvelope ann "Unbound variale"
      Fix (Ann _ e) -> traverse_ (go boundVars) e
