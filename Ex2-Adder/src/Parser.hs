{-# LANGUAGE OverloadedStrings  #-}

module Parser
  ( ExprA,
    Parser,
    parseExpr
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

type ExprA = Expr SourcePos

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

eNumber_ :: Parser ExprA
eNumber_ = do
  p <- pos
  lexeme_ $ ENumber p . fromIntegral <$> integer_

eIden_ :: Parser ExprA
eIden_ = EIden <$> pos <*> identifier_

expr_ :: Parser ExprA
expr_ = choice
  [ try ePrim_
  , elet_
  , eNumber_
  , eIden_
  ]

ePrim_ :: Parser ExprA
ePrim_ = do
  p <- pos
  void lparen_
  prim <- prim_
  expr <- expr_
  void rparen_
  lexeme_ $ pure $ EPrim1 p prim expr

prim_ :: Parser Prim1
prim_ = add1_ <|> sub1_

elet_ :: Parser ExprA
elet_ = do
  p <- pos
  void lparen_
  let_
  void lparen_
  binds <- some bind
  void rparen_
  inExpr <- expr_
  void rparen_
  lexeme_ $ pure $ ELet p binds inExpr
    where
      bind = do
        void lparen_
        var <- identifier_
        e <- expr_
        void rparen_
        pure (var, e)

identifier_ :: Parser Text
identifier_ = lexeme_ $ T.pack <$> some alphaNumChar

let_ :: Parser ()
let_ = void $ lexeme_ $ string "let"

add1_ :: Parser Prim1
add1_ = lexeme_ $ PAdd1 <$ string "add1"

sub1_ :: Parser Prim1
sub1_ = lexeme_ $ PSub1 <$ string "sub1"

lparen_ :: Parser Char
lparen_ = lexeme_ $ char '('

rparen_ :: Parser Char
rparen_ = lexeme_ $ char ')'

pos :: Parser SourcePos
pos = getSourcePos
