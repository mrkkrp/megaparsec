-- |
-- Module      :  Text.Megaparsec.Stream
-- Copyright   :  © 2015–2017 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Megaparsec's input stream facilities.
--
-- You probably do not want to import this module directly because
-- "Text.Megaparsec" re-exports it anyway.
--
-- @since 6.0.0

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Text.Megaparsec.Stream
  ( Stream (..) )
where

import Data.List (foldl')
import Data.Proxy
import Data.Semigroup ((<>))
import Data.Word (Word8)
import Text.Megaparsec.Pos
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Type class for inputs that can be consumed by the library.

class (Ord (Token s), Ord (Tokens s)) => Stream s where

  -- | Type of token in the stream.

  type Token s :: *

  -- | Type of “chunk” of the stream.

  type Tokens s :: *

  -- | Lift a single token to chunk of the stream. The default
  -- implementation is:
  --
  -- > tokenToChunk pxy = tokensToChunk pxy . pure
  --
  -- However for some types of stream there may be a more efficient way to
  -- lift.

  tokenToChunk  :: Proxy s -> Token s -> Tokens s
  tokenToChunk pxy = tokensToChunk pxy . pure

  -- | The first method that establishes isomorphism between list of tokens
  -- and chunk of the stream. Valid implementation should satisfy:
  --
  -- > chunkToTokens pxy (tokensToChunk pxy ts) == ts

  tokensToChunk :: Proxy s -> [Token s] -> Tokens s

  -- | The second method that establishes isomorphism between list of tokens
  -- and chunk of the stream. Valid implementation should satisfy:
  --
  -- > tokensToChunk pxy (chunkToTokens pxy chunk) == chunk

  chunkToTokens :: Proxy s -> Tokens s -> [Token s]

  -- | Return length of a chunk of the stream.

  chunkLength :: Proxy s -> Tokens s -> Int

  -- | Check if a chunk of the stream is empty. The default implementation
  -- is in terms of the more general 'chunkLength':
  --
  -- > chunkEmpty pxy ts = chunkLength pxy ts <= 0
  --
  -- However for many streams there may be a more efficient implementation.

  chunkEmpty :: Proxy s -> Tokens s -> Bool
  chunkEmpty pxy ts = chunkLength pxy ts <= 0
  {-# INLINE chunkEmpty #-}

  -- | Set source position __at__ given token. By default, the given
  -- 'SourcePos' (second argument) is just returned without looking at the
  -- token. This method is important when your stream is a collection of
  -- tokens where every token knows where it begins in the original input.

  positionAt1
    :: Proxy s         -- ^ 'Proxy' clarifying the type of stream
    -> SourcePos       -- ^ Current position
    -> Token s         -- ^ Current token
    -> SourcePos       -- ^ Position of the token
  positionAt1 Proxy = defaultPositionAt
  {-# INLINE positionAt1 #-}

  -- | The same as 'positionAt1', but for chunks of the stream. The function
  -- should return the position where the entire chunk begins. Again, by
  -- default the second argument is returned without modifications and the
  -- chunk is not looked at.

  positionAtN
    :: Proxy s         -- ^ 'Proxy' clarifying the type of stream
    -> SourcePos       -- ^ Current position
    -> Tokens s        -- ^ Current chunk
    -> SourcePos       -- ^ Position of the chunk
  positionAtN Proxy = defaultPositionAt
  {-# INLINE positionAtN #-}

  -- | Advance position given a single token. The returned position is the
  -- position right after the token, or position where the token ends.

  advance1
    :: Proxy s         -- ^ 'Proxy' clarifying the type of stream
    -> Pos             -- ^ Tab width
    -> SourcePos       -- ^ Current position
    -> Token s         -- ^ Current token
    -> SourcePos       -- ^ Advanced position

  -- | Advance position given a chunk of stream. The returned position is
  -- the position right after the chunk, or position where the chunk ends.

  advanceN
    :: Proxy s         -- ^ 'Proxy' clarifying the type of stream
    -> Pos             -- ^ Tab width
    -> SourcePos       -- ^ Current position
    -> Tokens s        -- ^ Current token
    -> SourcePos       -- ^ Advanced position

  -- | Extract a single token form the stream. Return 'Nothing' if the
  -- stream is empty.

  take1_ :: s -> Maybe (Token s, s)

  -- | @'takeN_' n s@ should try to extract a chunk of length @n@, or if the
  -- stream is too short, the rest of the stream. Valid implementation
  -- should follow the rules:
  --
  --     * If the requested length @n@ is 0 (or less), 'Nothing' should
  --       never be returned, instead @'Just' (\"\", s)@ should be returned,
  --       where @\"\"@ stands for the empty chunk, and @s@ is the original
  --       stream (second argument).
  --     * If the requested length is greater than 0 and the stream is
  --       empty, 'Nothing' should be returned indicating end of input.
  --     * In other cases, take chunk of length @n@ (or shorter if the
  --       stream is not long enough) from the input stream and return the
  --       chunk along with the rest of the stream.

  takeN_ :: Int -> s -> Maybe (Tokens s, s)

  -- | Extract chunk of the stream taking tokens while the supplied
  -- predicate returns 'True'. Return the chunk and the rest of the stream.
  --
  -- For many types of streams, the method allows for significant
  -- performance improvements, although it is not strictly necessary from
  -- conceptual point of view.

  takeWhile_ :: (Token s -> Bool) -> s -> (Tokens s, s)

instance Stream String where
  type Token String = Char
  type Tokens String = String
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = foldl' (defaultAdvance1 w)
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ("", s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span

instance Stream B.ByteString where
  type Token B.ByteString = Word8
  type Tokens B.ByteString = B.ByteString
  tokenToChunk Proxy = B.singleton
  tokensToChunk Proxy = B.pack
  chunkToTokens Proxy = B.unpack
  chunkLength Proxy = B.length
  chunkEmpty Proxy = B.null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = B.foldl' (defaultAdvance1 w)
  take1_ = B.uncons
  takeN_ n s
    | n <= 0    = Just (B.empty, s)
    | B.null s  = Nothing
    | otherwise = Just (B.splitAt n s)
  takeWhile_ = B.span

instance Stream BL.ByteString where
  type Token BL.ByteString = Word8
  type Tokens BL.ByteString = BL.ByteString
  tokenToChunk Proxy = BL.singleton
  tokensToChunk Proxy = BL.pack
  chunkToTokens Proxy = BL.unpack
  chunkLength Proxy = fromIntegral . BL.length
  chunkEmpty Proxy = BL.null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = BL.foldl' (defaultAdvance1 w)
  take1_ = BL.uncons
  takeN_ n s
    | n <= 0    = Just (BL.empty, s)
    | BL.null s = Nothing
    | otherwise = Just (BL.splitAt (fromIntegral n) s)
  takeWhile_ = BL.span

instance Stream T.Text where
  type Token T.Text = Char
  type Tokens T.Text = T.Text
  tokenToChunk Proxy = T.singleton
  tokensToChunk Proxy = T.pack
  chunkToTokens Proxy = T.unpack
  chunkLength Proxy = T.length
  chunkEmpty Proxy = T.null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = T.foldl' (defaultAdvance1 w)
  take1_ = T.uncons
  takeN_ n s
    | n <= 0    = Just (T.empty, s)
    | T.null s  = Nothing
    | otherwise = Just (T.splitAt n s)
  takeWhile_ = T.span

instance Stream TL.Text where
  type Token TL.Text  = Char
  type Tokens TL.Text = TL.Text
  tokenToChunk Proxy = TL.singleton
  tokensToChunk Proxy = TL.pack
  chunkToTokens Proxy = TL.unpack
  chunkLength Proxy = fromIntegral . TL.length
  chunkEmpty Proxy = TL.null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = TL.foldl' (defaultAdvance1 w)
  take1_ = TL.uncons
  takeN_ n s
    | n <= 0    = Just (TL.empty, s)
    | TL.null s = Nothing
    | otherwise = Just (TL.splitAt (fromIntegral n) s)
  takeWhile_ = TL.span

----------------------------------------------------------------------------
-- Helpers

-- | Default positioning function designed to work with simple streams where
-- tokens do not contain info about their position in the stream. Thus it
-- just returns the given 'SourcePos' without re-positioning.

defaultPositionAt :: SourcePos -> a -> SourcePos
defaultPositionAt pos _ = pos
{-# INLINE defaultPositionAt #-}

-- | Update a source position given a token. The first argument specifies
-- the tab width. If the character is a newline (\'\\n\') the line number is
-- incremented by 1 and column number is reset to 1. If the character is a
-- tab (\'\\t\') the column number is incremented to the nearest tab
-- position. In all other cases, the column is incremented by 1.

defaultAdvance1 :: Enum t
  => Pos               -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> t                 -- ^ Current token
  -> SourcePos         -- ^ Incremented position
defaultAdvance1 width (SourcePos n l c x) t = npos
  where
    w  = unPos width
    c' = unPos c
    x' = x <> pos1
    npos =
      case fromEnum t of
        10 -> SourcePos n (l <> pos1) pos1 x'
        9  -> SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)) x'
        _  -> SourcePos n l (c <> pos1) x'
{-# INLINE defaultAdvance1 #-}
