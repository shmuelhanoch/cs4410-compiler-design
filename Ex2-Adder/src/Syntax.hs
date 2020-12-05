module Syntax 
  ( Prim1(..),
    Expr(..),
    SrcLoc(..),
    ExprA
  ) 
where

import Data.Text (Text)


data SrcLoc
  = SrcLoc
  Text -- file name
  !Int -- line number
  !Int -- column number
  deriving (Eq, Show)

data Prim1
 = PAdd1
 | PSub1

data Expr ann
  = ENumber ann Int
  | EPrim1 ann Prim1 (Expr ann)
  | ELet ann [(Text, Expr ann)] (Expr ann)

type ExprA = Expr SrcLoc
