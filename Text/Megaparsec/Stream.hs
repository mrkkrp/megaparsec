-- |
-- Module      :  Text.Megaparsec.Stream
-- Copyright   :  © 2015–2019 Megaparsec contributors
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

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Text.Megaparsec.Stream
  ( Stream (..) )
where

import Data.Char (chr)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Word (Word8)
import Text.Megaparsec.Pos
import Text.Megaparsec.State
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List.NonEmpty         as NE
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
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

  -- | Pretty-print non-empty stream of tokens. This function is also used
  -- to print single tokens (represented as singleton lists).
  --
  -- @since 7.0.0

  showTokens :: Proxy s -> NonEmpty (Token s) -> String

  -- | Given an offset @o@ and initial 'PosState', adjust the state in such
  -- a way that it starts at the offset.
  --
  -- Return three values (in order):
  --
  --     * 'SourcePos' which the given offset @o@ points to.
  --     * 'String' representing the line on which the given offset @o@ is
  --       located. The line should satisfy a number of conditions that are
  --       described below.
  --     * The updated 'PosState' which can be in turn used to locate
  --       another offset @o'@ given that @o' >= o@.
  --
  -- The 'String' representing the offending line in input stream should
  -- satisfy the following:
  --
  --     * It should adequately represent location of token at the offset of
  --       interest, that is, character at 'sourceColumn' of the returned
  --       'SourcePos' should correspond to the token at the offset @o@.
  --     * It should not include the newline at the end.
  --     * It should not be empty, if the line happens to be empty, it
  --       should be replaced with the string @\"\<empty line\>\"@.
  --     * Tab characters should be replaced by appropriate number of
  --       spaces, which is determined by the 'pstateTabWidth' field of
  --       'PosState'.
  --
  -- @since 7.0.0

  reachOffset
    :: Int             -- ^ Offset to reach
    -> PosState s      -- ^ Initial 'PosState' to use
    -> (SourcePos, String, PosState s) -- ^ (See below)

  -- | A version of 'reachOffset' that may be faster because it doesn't need
  -- to fetch the line at which the given offset in located.
  --
  -- The default implementation is this:
  --
  -- > reachOffsetNoLine o pst =
  -- >   let (spos, _, pst')=  reachOffset o pst
  -- >   in (spos, pst')
  --
  -- @since 7.0.0

  reachOffsetNoLine
    :: Int             -- ^ Offset to reach
    -> PosState s      -- ^ Initial 'PosState' to use
    -> (SourcePos, PosState s) -- ^ Reached source position and updated state
  reachOffsetNoLine o pst =
    let (spos, _, pst') = reachOffset o pst
    in (spos, pst')

instance Stream String where
  type Token String = Char
  type Tokens String = String
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ("", s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span
  showTokens Proxy = stringPretty
  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset o pst =
    reachOffset' splitAt foldl' id id ('\n','\t') o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' splitAt foldl' ('\n', '\t') o pst

instance Stream B.ByteString where
  type Token B.ByteString = Word8
  type Tokens B.ByteString = B.ByteString
  tokenToChunk Proxy = B.singleton
  tokensToChunk Proxy = B.pack
  chunkToTokens Proxy = B.unpack
  chunkLength Proxy = B.length
  chunkEmpty Proxy = B.null
  take1_ = B.uncons
  takeN_ n s
    | n <= 0    = Just (B.empty, s)
    | B.null s  = Nothing
    | otherwise = Just (B.splitAt n s)
  takeWhile_ = B.span
  showTokens Proxy = stringPretty . fmap (chr . fromIntegral)
  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset o pst =
    reachOffset' B.splitAt B.foldl' B8.unpack (chr . fromIntegral) (10, 9) o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' B.splitAt B.foldl' (10, 9) o pst

instance Stream BL.ByteString where
  type Token BL.ByteString = Word8
  type Tokens BL.ByteString = BL.ByteString
  tokenToChunk Proxy = BL.singleton
  tokensToChunk Proxy = BL.pack
  chunkToTokens Proxy = BL.unpack
  chunkLength Proxy = fromIntegral . BL.length
  chunkEmpty Proxy = BL.null
  take1_ = BL.uncons
  takeN_ n s
    | n <= 0    = Just (BL.empty, s)
    | BL.null s = Nothing
    | otherwise = Just (BL.splitAt (fromIntegral n) s)
  takeWhile_ = BL.span
  showTokens Proxy = stringPretty . fmap (chr . fromIntegral)
  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset o pst =
    reachOffset' splitAtBL BL.foldl' BL8.unpack (chr . fromIntegral) (10, 9) o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' splitAtBL BL.foldl' (10, 9) o pst

instance Stream T.Text where
  type Token T.Text = Char
  type Tokens T.Text = T.Text
  tokenToChunk Proxy = T.singleton
  tokensToChunk Proxy = T.pack
  chunkToTokens Proxy = T.unpack
  chunkLength Proxy = T.length
  chunkEmpty Proxy = T.null
  take1_ = T.uncons
  takeN_ n s
    | n <= 0    = Just (T.empty, s)
    | T.null s  = Nothing
    | otherwise = Just (T.splitAt n s)
  takeWhile_ = T.span
  showTokens Proxy = stringPretty
  -- NOTE Do not eta-reduce (breaks inlining of reachOffset').
  reachOffset o pst =
    reachOffset' T.splitAt T.foldl' T.unpack id ('\n', '\t') o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' T.splitAt T.foldl' ('\n', '\t') o pst

instance Stream TL.Text where
  type Token TL.Text  = Char
  type Tokens TL.Text = TL.Text
  tokenToChunk Proxy = TL.singleton
  tokensToChunk Proxy = TL.pack
  chunkToTokens Proxy = TL.unpack
  chunkLength Proxy = fromIntegral . TL.length
  chunkEmpty Proxy = TL.null
  take1_ = TL.uncons
  takeN_ n s
    | n <= 0    = Just (TL.empty, s)
    | TL.null s = Nothing
    | otherwise = Just (TL.splitAt (fromIntegral n) s)
  takeWhile_ = TL.span
  showTokens Proxy = stringPretty
  -- NOTE Do not eta-reduce (breaks inlining of reachOffset').
  reachOffset o pst =
    reachOffset' splitAtTL TL.foldl' TL.unpack id ('\n', '\t') o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' splitAtTL TL.foldl' ('\n', '\t') o pst

----------------------------------------------------------------------------
-- Helpers

-- | An internal helper state type combining a difference 'String' and an
-- unboxed 'SourcePos'.

data St = St SourcePos ShowS

-- | A helper definition to facilitate defining 'reachOffset' for various
-- stream types.

reachOffset'
  :: forall s. Stream s
  => (Int -> s -> (Tokens s, s))
     -- ^ How to split input stream at given offset
  -> (forall b. (b -> Token s -> b) -> b -> Tokens s -> b)
     -- ^ How to fold over input stream
  -> (Tokens s -> String)
     -- ^ How to convert chunk of input stream into a 'String'
  -> (Token s -> Char)
     -- ^ How to convert a token into a 'Char'
  -> (Token s, Token s)
     -- ^ Newline token and tab token
  -> Int
     -- ^ Offset to reach
  -> PosState s
     -- ^ Initial 'PosState' to use
  -> (SourcePos, String, PosState s)
     -- ^ Reached 'SourcePos', line at which 'SourcePos' is located, updated
     -- 'PosState'
reachOffset' splitAt'
             foldl''
             fromToks
             fromTok
             (newlineTok, tabTok)
             o
             PosState {..} =
  ( spos
  , case expandTab pstateTabWidth
           . addPrefix
           . f
           . fromToks
           . fst
           $ takeWhile_ (/= newlineTok) post of
      "" -> "<empty line>"
      xs -> xs
  , PosState
      { pstateInput = post
      , pstateOffset = max pstateOffset o
      , pstateSourcePos = spos
      , pstateTabWidth = pstateTabWidth
      , pstateLinePrefix =
          if sameLine
            -- NOTE We don't use difference lists here because it's
            -- desirable for 'PosState' to be an instance of 'Eq' and
            -- 'Show'. So we just do appending here. Fortunately several
            -- parse errors on the same line should be relatively rare.
            then pstateLinePrefix ++ f ""
            else f ""
      }
  )
  where
    addPrefix xs =
      if sameLine
        then pstateLinePrefix ++ xs
        else xs
    sameLine = sourceLine spos == sourceLine pstateSourcePos
    (pre, post) = splitAt' (o - pstateOffset) pstateInput
    St spos f = foldl'' go (St pstateSourcePos id) pre
    go (St apos g) ch =
      let SourcePos n l c = apos
          c' = unPos c
          w  = unPos pstateTabWidth
      in if | ch == newlineTok ->
                St (SourcePos n (l <> pos1) pos1)
                   id
            | ch == tabTok ->
                St (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
                   (g . (fromTok ch :))
            | otherwise ->
                St (SourcePos n l (c <> pos1))
                   (g . (fromTok ch :))
{-# INLINE reachOffset' #-}

-- | Like 'reachOffset'' but for 'reachOffsetNoLine'.

reachOffsetNoLine'
  :: forall s. Stream s
  => (Int -> s -> (Tokens s, s))
     -- ^ How to split input stream at given offset
  -> (forall b. (b -> Token s -> b) -> b -> Tokens s -> b)
     -- ^ How to fold over input stream
  -> (Token s, Token s)
     -- ^ Newline token and tab token
  -> Int
     -- ^ Offset to reach
  -> PosState s
     -- ^ Initial 'PosState' to use
  -> (SourcePos, PosState s)
     -- ^ Reached 'SourcePos' and updated 'PosState'
reachOffsetNoLine' splitAt'
                   foldl''
                   (newlineTok, tabTok)
                   o
                   PosState {..} =
  ( spos
  , PosState
      { pstateInput = post
      , pstateOffset = max pstateOffset o
      , pstateSourcePos = spos
      , pstateTabWidth = pstateTabWidth
      , pstateLinePrefix = pstateLinePrefix
      }
  )
  where
    spos = foldl'' go pstateSourcePos pre
    (pre, post) = splitAt' (o - pstateOffset) pstateInput
    go (SourcePos n l c) ch =
      let c' = unPos c
          w  = unPos pstateTabWidth
      in if | ch == newlineTok ->
                SourcePos n (l <> pos1) pos1
            | ch == tabTok ->
                SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w))
            | otherwise ->
                SourcePos n l (c <> pos1)
{-# INLINE reachOffsetNoLine' #-}

-- | Like 'BL.splitAt' but accepts the index as an 'Int'.

splitAtBL :: Int -> BL.ByteString -> (BL.ByteString, BL.ByteString)
splitAtBL n = BL.splitAt (fromIntegral n)
{-# INLINE splitAtBL #-}

-- | Like 'TL.splitAt' but accepts the index as an 'Int'.

splitAtTL :: Int -> TL.Text -> (TL.Text, TL.Text)
splitAtTL n = TL.splitAt (fromIntegral n)
{-# INLINE splitAtTL #-}

-- | @stringPretty s@ returns pretty representation of string @s@. This is
-- used when printing string tokens in error messages.

stringPretty :: NonEmpty Char -> String
stringPretty (x:|[])      = charPretty x
stringPretty ('\r':|"\n") = "crlf newline"
stringPretty xs           = "\"" <> concatMap f (NE.toList xs) <> "\""
  where
    f ch =
      case charPretty' ch of
        Nothing     -> [ch]
        Just pretty -> "<" <> pretty <> ">"

-- | @charPretty ch@ returns user-friendly string representation of given
-- character @ch@, suitable for using in error messages.

charPretty :: Char -> String
charPretty ' ' = "space"
charPretty ch = fromMaybe ("'" <> [ch] <> "'") (charPretty' ch)

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.

charPretty' :: Char -> Maybe String
charPretty' = \case
  '\NUL' -> Just "null"
  '\SOH' -> Just "start of heading"
  '\STX' -> Just "start of text"
  '\ETX' -> Just "end of text"
  '\EOT' -> Just "end of transmission"
  '\ENQ' -> Just "enquiry"
  '\ACK' -> Just "acknowledge"
  '\BEL' -> Just "bell"
  '\BS'  -> Just "backspace"
  '\t'   -> Just "tab"
  '\n'   -> Just "newline"
  '\v'   -> Just "vertical tab"
  '\f'   -> Just "form feed"
  '\r'   -> Just "carriage return"
  '\SO'  -> Just "shift out"
  '\SI'  -> Just "shift in"
  '\DLE' -> Just "data link escape"
  '\DC1' -> Just "device control one"
  '\DC2' -> Just "device control two"
  '\DC3' -> Just "device control three"
  '\DC4' -> Just "device control four"
  '\NAK' -> Just "negative acknowledge"
  '\SYN' -> Just "synchronous idle"
  '\ETB' -> Just "end of transmission block"
  '\CAN' -> Just "cancel"
  '\EM'  -> Just "end of medium"
  '\SUB' -> Just "substitute"
  '\ESC' -> Just "escape"
  '\FS'  -> Just "file separator"
  '\GS'  -> Just "group separator"
  '\RS'  -> Just "record separator"
  '\US'  -> Just "unit separator"
  '\DEL' -> Just "delete"
  '\160' -> Just "non-breaking space"
  _      -> Nothing

-- | Replace tab characters with given number of spaces.

expandTab
  :: Pos
  -> String
  -> String
expandTab w' = go 0
  where
    go 0 []        = []
    go 0 ('\t':xs) = go w xs
    go 0 (x:xs)    = x : go 0 xs
    go n xs        = ' ' : go (n - 1) xs
    w              = unPos w'
