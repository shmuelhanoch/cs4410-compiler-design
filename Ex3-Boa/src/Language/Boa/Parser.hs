{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.Boa.Parser
  ( ExprA,
    parseExpr,
  )
where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
  ( Operator (InfixL),
    makeExprParser,
  )
import Data.Bifunctor (Bifunctor (first))
import Data.Generics.Fixplate.Base
  ( Ann (Ann),
    Attr,
    Mu (Fix),
    attribute,
  )
import Data.Proxy (Proxy (..))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    PosState (pstateSourcePos),
    SourcePos (sourceColumn),
    Token,
    choice,
    getSourcePos,
    runParser,
    sepBy1,
    some,
    sourceLine,
    sourceName,
    tokensLength,
    unPos,
  )
import Text.Megaparsec.Char (alphaNumChar, char, space1)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Error
  ( ErrorFancy (ErrorCustom),
    ErrorItem (Tokens),
    ParseError (FancyError, TrivialError),
    ParseErrorBundle (..),
    ShowErrorComponent (errorComponentLen),
    errorOffset,
    parseErrorTextPretty,
  )
import Text.Megaparsec.Stream (TraversableStream, VisualStream, reachOffset)

import Language.Boa.Syntax (ExprF (..), Prim1 (..), Prim2 (..))
import Language.Boa.Error (Messages, MsgEnvelope (MsgEnvelope), addMessage, emptyMsgs)
import Language.Boa.SrcLoc
  ( SrcSpan (SrcSpan),
    fromPos,
    mkSrcSpan,
    srcSpanEnd,
    srcSpanStart,
  )

type ExprA = Attr ExprF SrcSpan

type Parser = Parsec Void Text

parseExpr :: FilePath -> Text -> Either Messages ExprA
parseExpr file =
  first errorBundleToMessagesEnvelope
    . runParser pExpr file

pExpr :: Parser ExprA
pExpr =
  choice
    [ pELet
    , pEIf
    , pBinOpExpr
    ]

pELet :: Parser ExprA
pELet = lexeme $ do
  strt <- fromPos <$> getSourcePos
  void $ lexeme "let"
  binds <- pBind `sepBy1` lexeme (char ',')
  void $ lexeme "in"
  inExpr <- pExpr
  end <- fromPos <$> getSourcePos
  pure $ Fix $ Ann (mkSrcSpan strt end) (ELet binds inExpr)
  where
    pBind = lexeme $ do
      var <- pIdentifier <* sc
      void $ lexeme "="
      (var,) <$> pExpr

pEIf :: Parser ExprA
pEIf = lexeme $ do
  strt <- fromPos <$> getSourcePos
  void $ lexeme "if"
  cond <- pExpr
  void $ char ':'
  thenBranch <- pExpr
  void $ lexeme "else:"
  elseBranch <- pExpr
  end <- fromPos <$> getSourcePos
  pure $ Fix $ Ann (mkSrcSpan strt end) (EIf cond thenBranch elseBranch)

pBinOpExpr :: Parser ExprA
pBinOpExpr = makeExprParser pTerm opTable

pTerm :: Parser ExprA
pTerm =
  choice
    [ parens pExpr
    , pAdd1
    , pSub1
    , pENumber
    , pEId
    ]

opTable :: [[Operator Parser ExprA]]
opTable =
  [
    [ binary "*" Times
    , binary "+" Add
    , binary "-" Sub
    ]
  ]
  where
    binary name prim = InfixL $ do
      void $ symbol name
      pure $ \e1 e2 ->
        Fix $
          Ann
            (mkSrcSpan (srcSpanStart $ attribute e1) (srcSpanEnd $ attribute e2))
            (EPrim2 prim e1 e2)

pAdd1 :: Parser ExprA
pAdd1 = lexeme $ do
  strt <- fromPos <$> getSourcePos
  void $ lexeme "add1"
  e <- parens pExpr
  end <- fromPos <$> getSourcePos
  pure $ Fix $ Ann (mkSrcSpan strt end) (EPrim1 Add1 e)

pSub1 :: Parser ExprA
pSub1 = lexeme $ do
  strt <- fromPos <$> getSourcePos
  void $ lexeme "sub1"
  e <- parens pExpr
  end <- fromPos <$> getSourcePos
  pure $ Fix $ Ann (mkSrcSpan strt end) (EPrim1 Sub1 e)

pENumber :: Parser ExprA
pENumber = lexeme $ do
  strt <- fromPos <$> getSourcePos
  x <- pInteger
  end <- fromPos <$> getSourcePos
  sc
  pure $ Fix $ Ann (mkSrcSpan strt end) (ENumber $ fromIntegral x)

pEId :: Parser ExprA
pEId = lexeme $ do
  strt <- fromPos <$> getSourcePos
  x <- pIdentifier
  end <- fromPos <$> getSourcePos
  sc
  pure $ Fix $ Ann (mkSrcSpan strt end) (EId x)

pInteger :: Parser Integer
pInteger = L.decimal

pIdentifier :: Parser Text
pIdentifier = T.pack <$> some alphaNumChar

-- Some Megaparsec related helpers

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- General helpers

parens :: Parser a -> Parser a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

errorBundleToMessagesEnvelope ::
  forall s e.
  ( VisualStream s
  , TraversableStream s
  , ShowErrorComponent e
  ) =>
  ParseErrorBundle s e ->
  Messages
errorBundleToMessagesEnvelope ParseErrorBundle{..} =
  fst $ foldl f (emptyMsgs, bundlePosState) bundleErrors
  where
    f (msgs, !pst) e = (addMessage msg msgs, pst')
      where
        (msline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        file = sourceName epos
        line = unPos $ sourceLine epos
        startCol = unPos $ sourceColumn epos
        endCol =
          if startCol + elen > slineLen
            then slineLen
            else startCol + elen
        srcSpan = SrcSpan file line startCol line endCol
        msg = MsgEnvelope srcSpan $ T.pack $ parseErrorTextPretty e
        slineLen = maybe 0 length msline
        pxy = Proxy :: Proxy s
        elen =
          case e of
            TrivialError _ Nothing _ -> 1
            TrivialError _ (Just x) _ -> errorItemLength pxy x
            FancyError _ xs ->
              S.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

-- | Get length of the “pointer” to display under a given 'ErrorItem'.
errorItemLength :: VisualStream s => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1
