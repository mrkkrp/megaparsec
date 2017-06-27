-- |
-- Module      :  Text.Megaparsec.Stream
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Megaparsec's input stream facilities.
--
-- You probably do not want to import this module because "Text.Megaparsec"
-- re-exports it anyway.

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Text.Megaparsec.Stream
  ( Stream (..) )
where

import Data.Proxy
import Data.Semigroup ((<>))
import Text.Megaparsec.Pos
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL

-- | An instance of @Stream s@ has stream type @s@. Token type is determined
-- by the stream and can be found via 'Token' type function.

class Ord (Token s) => Stream s where

  -- | Type of token in stream.
  --
  -- @since 5.0.0

  type Token s :: *

  -- | Get next token from the stream. If the stream is empty, return
  -- 'Nothing'.

  uncons :: s -> Maybe (Token s, s)

  -- | Update position in stream given tab width, current position, and
  -- current token. The result is a tuple where the first element will be
  -- used to report parse errors for current token, while the second element
  -- is the incremented position that will be stored in the parser's state.
  -- The stored (incremented) position is used whenever position can't
  -- be\/shouldn't be updated by consuming a token. For example, when using
  -- 'failure', we don't grab a new token (we need to fail right were we are
  -- now), so error position will be taken from parser's state.
  --
  -- When you work with streams where elements do not contain information
  -- about their position in input, the result is usually consists of the
  -- third argument unchanged and incremented position calculated with
  -- respect to current token. This is how default instances of 'Stream'
  -- work (they use 'defaultUpdatePos', which may be a good starting point
  -- for your own position-advancing function).
  --
  -- When you wish to deal with a stream of tokens where every token “knows”
  -- its start and end position in input (for example, you have produced the
  -- stream with Happy\/Alex), then the best strategy is to use the start
  -- position as the actual element position and provide the end position of
  -- the token as the incremented one.
  --
  -- @since 5.0.0

  updatePos
    :: Proxy s -- ^ Proxy clarifying stream type ('Token' is not injective)
    -> Pos             -- ^ Tab width
    -> SourcePos       -- ^ Current position
    -> Token s         -- ^ Current token
    -> (SourcePos, SourcePos) -- ^ Actual position and incremented position

instance Stream String where
  type Token String = Char
  uncons [] = Nothing
  uncons (t:ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

instance Stream B.ByteString where
  type Token B.ByteString = Char
  uncons = B.uncons
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

instance Stream BL.ByteString where
  type Token BL.ByteString = Char
  uncons = BL.uncons
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

instance Stream T.Text where
  type Token T.Text = Char
  uncons = T.uncons
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

instance Stream TL.Text where
  type Token TL.Text = Char
  uncons = TL.uncons
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

-- | Update a source position given a character. The first argument
-- specifies the tab width. If the character is a newline (\'\\n\') the line
-- number is incremented by 1. If the character is a tab (\'\\t\') the
-- column number is incremented to the nearest tab position. In all other
-- cases, the column is incremented by 1.

defaultUpdatePos
  :: Pos               -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> Char              -- ^ Current token
  -> (SourcePos, SourcePos) -- ^ Actual position and incremented position
defaultUpdatePos width apos@(SourcePos n l c) ch = (apos, npos)
  where
    w  = unPos width
    c' = unPos c
    npos =
      case ch of
        '\n' -> SourcePos n (l <> pos1) pos1
        '\t' -> SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w))
        _    -> SourcePos n l (c <> pos1)
