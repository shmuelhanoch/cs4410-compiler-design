{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Types and utils related to error handling.
module Language.Boa.Error
  ( MsgEnvelope (..),
    Messages (..),
    emptyMsgs,
    singleMsg,
    addMessage,
    reportDiagnostic,
    reportDiagnostics,
    showMsgs,
  )
where

import Control.Exception (catch)
import Prettyprinter
  ( Doc,
    Pretty (pretty),
    annotate,
    colon,
    hang,
    hcat,
    line,
    space,
    vsep,
    (<+>),
  )
import Prettyprinter.Render.Terminal
  ( AnsiStyle,
    Color (Blue, Red),
    bold,
    color,
  )
import Language.Boa.SrcLoc
  ( SrcSpan
      ( srcSpanEndCol,
        srcSpanEndLine,
        srcSpanFile,
        srcSpanStartCol,
        srcSpanStartLine
      ),
  )
import System.IO (hPrint, hPutChar, stderr)
import Data.Text (Text)
import Data.Foldable (traverse_)

data MsgEnvelope = MsgEnvelope
  { errMsgSpan :: SrcSpan
  , errMsgDiagnostic :: Text
  }

newtype Messages = Messages {getMessages :: [MsgEnvelope]}

emptyMsgs :: Messages
emptyMsgs = Messages []

singleMsg :: MsgEnvelope -> Messages
singleMsg = Messages . (:[])

addMessage :: MsgEnvelope -> Messages -> Messages
addMessage m ms = Messages $ m : getMessages ms

-- show error messages without the pretty location info
showMsgs :: Messages -> String
showMsgs = unlines . map (show . errMsgDiagnostic) . getMessages

reportDiagnostic :: MsgEnvelope -> IO ()
reportDiagnostic (MsgEnvelope srcSpan msg) = do
  hPutChar stderr '\n'
  caretDiagnostic <- getCaretDiagnostic srcSpan
  hPrint stderr (vsep [mkLocMessageAnn srcSpan $ pretty msg, caretDiagnostic])

reportDiagnostics :: Messages -> IO ()
reportDiagnostics = traverse_ reportDiagnostic . getMessages

-- | Make a possibly annotated error message with location info.
mkLocMessageAnn ::
  SrcSpan ->
  Doc AnsiStyle ->
  Doc AnsiStyle
mkLocMessageAnn locn msg =
  let locn' = pretty $ show locn
      header = locn' <> colon <+> annotate errorColor "error:"
   in header <+> hang 4 msg

marginColor, errorColor :: AnsiStyle
marginColor = bold <> color Blue
errorColor = bold <> color Red

getCaretDiagnostic :: SrcSpan -> IO (Doc AnsiStyle)
getCaretDiagnostic spn =
  caretDiagnostic <$> getSrcLine (srcSpanFile spn) row
  where
    getSrcLine file i =
      (Just . (!! i) . lines <$> readFile file)
        `catch` \(_ :: IOError) ->
          pure Nothing

    -- allow user to visibly see that their code is incorrectly encoded
    -- (StringBuffer.nextChar uses \0 to represent undecodable characters)

    row = srcSpanStartLine spn
    rowStr = show row
    multiline = row /= srcSpanEndLine spn

    caretDiagnostic Nothing = mempty
    caretDiagnostic (Just srcLineWithNewline) =
      hcat
        [ annotate marginColor $ pretty marginSpace
        , line
        , annotate marginColor $ pretty marginRow
        , space <> pretty srcLinePre
        , annotate errorColor $ pretty srcLineSpan
        , pretty srcLinePost <> line
        , annotate marginColor $ pretty marginSpace
        , annotate errorColor space <> pretty caretLine
        ]
      where
        -- expand tabs in a device-independent manner #13664
        expandTabs tabWidth i s =
          case s of
            "" -> ""
            '\t' : cs ->
              replicate effectiveWidth ' '
                ++ expandTabs tabWidth (i + effectiveWidth) cs
            c : cs -> c : expandTabs tabWidth (i + 1) cs
          where
            effectiveWidth = tabWidth - i `mod` tabWidth

        srcLine = filter (/= '\n') (expandTabs 8 0 srcLineWithNewline)

        start = srcSpanStartCol spn - 1
        end
          | multiline = length srcLine
          | otherwise = srcSpanEndCol spn - 1
        width = max 1 (end - start)

        marginWidth = length rowStr
        marginSpace = replicate marginWidth ' ' ++ " |"
        marginRow = rowStr ++ " |"

        (srcLinePre, srcLineRest) = splitAt start srcLine
        (srcLineSpan, srcLinePost) = splitAt width srcLineRest

        caretEllipsis
          | multiline = "..."
          | otherwise = ""
        caretLine = replicate start ' ' ++ replicate width '^' ++ caretEllipsis
