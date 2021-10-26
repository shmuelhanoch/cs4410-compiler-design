{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Boa.Syntax
  ( Prim1(..),
    Prim2(..),
    ExprF(..),
    Expr,
  )
where

import Data.Text (Text)
import Data.Generics.Fixplate.Base
    ( EqF(..), Mu, OrdF(..), ShowF(..) )
import Data.Data (Data, Typeable)

data Prim1
 = Add1
 | Sub1
  deriving (Eq, Ord, Show, Data, Typeable)

data Prim2
  = Add
  | Sub
  | Times
  deriving (Eq, Ord, Show, Data, Typeable)

data ExprF e
  = ELet [(Text, e)] e
  | EPrim1 Prim1 e
  | EPrim2 Prim2 e e
  | EIf e e e
  | ENumber Int
  | EId Text
  deriving (Eq, Ord, Show, Data, Typeable, Functor, Foldable, Traversable)

instance EqF ExprF where equalF = (==)
instance OrdF ExprF where compareF = compare
instance ShowF ExprF where showsPrecF = showsPrec

type Expr = Mu ExprF
