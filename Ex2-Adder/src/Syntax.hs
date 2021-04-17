module Syntax 
  ( Prim1(..),
    Expr(..),
  )
where

import Data.Text (Text)

data Prim1
 = PAdd1
 | PSub1
  deriving (Eq, Show)

data Expr ann
  = ENumber ann Int
  | EPrim1 ann Prim1 (Expr ann)
  | EIden ann Text
  | ELet ann [(Text, Expr ann)] (Expr ann)
  deriving (Eq, Show)
