-- | This module Language.Boa.contains types and utilities that relate to the positions of
-- things in source files.
module Language.Boa.SrcLoc where

import qualified Text.Megaparsec as MP

-- | Source Location
data SrcLoc = SrcLoc
  { srcLocFile :: !FilePath
  , srcLocLine :: {-# UNPACK #-} !Int
  , srcLocCol :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

-- | Source Span
--
-- A 'SrcSpan' identifies a specific portion of a text file
data SrcSpan = SrcSpan
  { srcSpanFile :: !FilePath
  , srcSpanStartLine :: {-# UNPACK #-} !Int
  , srcSpanStartCol :: {-# UNPACK #-} !Int
  , srcSpanEndLine :: {-# UNPACK #-} !Int
  , srcSpanEndCol :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

mkSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
mkSrcSpan loc1 loc2 =
  SrcSpan
    (srcLocFile loc1)
    (srcLocLine loc1)
    (srcLocCol loc1)
    (srcLocLine loc2)
    (srcLocCol loc2)

srcSpanStart :: SrcSpan -> SrcLoc
srcSpanStart x = SrcLoc (srcSpanFile x) (srcSpanStartLine x) (srcSpanStartCol x)

srcSpanEnd :: SrcSpan -> SrcLoc
srcSpanEnd x = SrcLoc (srcSpanFile x) (srcSpanEndLine x) (srcSpanEndCol x)

fromPos :: MP.SourcePos -> SrcLoc
fromPos p =
  SrcLoc
    (MP.sourceName p)
    (MP.unPos $ MP.sourceLine p)
    (MP.unPos $ MP.sourceColumn p)

