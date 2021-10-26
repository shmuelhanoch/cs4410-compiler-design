
{-# LANGUAGE LambdaCase #-}

-- | The tagging stage giving each node in the AST a unique numerical tag.

module Language.Boa.Tag (Tag(..), tag) where

import Data.Generics.Fixplate.Attributes (enumerateNodes_, annMap)
import Data.Generics.Fixplate.Base (Attr, forget)

import Language.Boa.Syntax (ExprF (..))

newtype Tag  = MkTag { unTag :: Int }

tag :: Attr ExprF a -> Attr ExprF Tag
tag  = annMap MkTag . enumerateNodes_ . forget
