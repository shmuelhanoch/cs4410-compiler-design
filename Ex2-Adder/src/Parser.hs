{-# LANGUAGE OverloadedStrings  #-}

module Parser
  ( Parser,
    parseExpr,
  )
where

import Syntax

import Control.Monad
import Data.Bifunctor
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseExpr :: Text -> Either Text ExprA
parseExpr
  = first (T.pack . errorBundlePretty)
  . runParser expr_ ""

sc_ :: Parser ()
sc_ = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme_ :: Parser a -> Parser a
lexeme_ = L.lexeme sc_

integer_ :: Parser Integer
integer_ = lexeme_ L.decimal

enumber_ :: Parser ExprA
enumber_ = do
  pos <- posToSrcLoc <$> getSourcePos
  ENumber pos . fromIntegral <$> integer_


expr_ :: Parser ExprA
expr_ = enumber_ <|> eprim_ <|> elet_

eprim_ :: Parser ExprA
eprim_ = do
  pos <- posToSrcLoc <$> getSourcePos
  prim <- prim_
  EPrim1 pos prim <$> expr_

prim_ :: Parser Prim1
prim_ = add_ <|> sub_

elet_ :: Parser ExprA
elet_ = do
  pos <- posToSrcLoc <$> getSourcePos
  void lparen_
  bindList <- some letbind_
  void rparen_
  ELet pos bindList <$> expr_

letbind_ :: Parser (Text, ExprA)
letbind_ = do
  void lparen_
  name <- string
  void space1
  expr <- expr_
  pure $ (name, expr)

add_ :: Parser Prim1
add_ = undefined

sub_ :: Parser Prim1
sub_ = undefined

lparen_ :: Parser Char
lparen_ = char "("

rparen_ :: Parser Char
rparen_ = char ")"

{-
data Prim1
 = PAdd1
 | PSub1

data Expr ann
  = ENumber ann Int
  | EPrim1 ann Prim1 (Expr ann)
  | ELet ann [(Text, Expr ann)] (Expr ann)
-}

posToSrcLoc :: SourcePos -> SrcLoc
posToSrcLoc (SourcePos file linePos colPos) = SrcLoc
  (T.pack file)
  (unPos linePos)
  (unPos colPos)
