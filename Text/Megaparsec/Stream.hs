{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Text.Megaparsec.Stream
-- Copyright   :  © 2015–present Megaparsec contributors
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
module Text.Megaparsec.Stream
  ( Stream (..),
    ShareInput (..),
    NoShareInput (..),
    VisualStream (..),
    TraversableStream (..),
  )
where

import Data.Bifunctor (second)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (chr)
import Data.Foldable (foldl', toList)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Proxy
import qualified Data.Sequence as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word8)
import Text.Megaparsec.Pos
import Text.Megaparsec.State
import qualified Text.Megaparsec.Unicode as Unicode

-- | Type class for inputs that can be consumed by the library.
--
-- Note that the 'Stream' instances for 'Text' and 'ByteString' (strict and
-- lazy) default to "input sharing" (see 'ShareInput', 'NoShareInput'). We plan
-- to move away from input sharing in a future major release; if you want to
-- retain the current behaviour and are concerned with maximum performance you
-- should consider using the 'ShareInput' wrapper explicitly.
--
-- __Note__: before the version /9.0.0/ the class included the methods from
-- 'VisualStream' and 'TraversableStream'.
class (Ord (Token s), Ord (Tokens s)) => Stream s where
  -- | Type of token in the stream.
  type Token s :: Type

  -- | Type of “chunk” of the stream.
  type Tokens s :: Type

  -- | Lift a single token to chunk of the stream. The default
  -- implementation is:
  --
  -- > tokenToChunk pxy = tokensToChunk pxy . pure
  --
  -- However for some types of stream there may be a more efficient way to
  -- lift.
  tokenToChunk :: Proxy s -> Token s -> Tokens s
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

-- | @since 9.0.0
instance (Ord a) => Stream [a] where
  type Token [a] = a
  type Tokens [a] = [a]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ [] = Nothing
  take1_ (t : ts) = Just (t, ts)
  takeN_ n s
    | n <= 0 = Just ([], s)
    | null s = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span

-- | @since 9.0.0
instance (Ord a) => Stream (S.Seq a) where
  type Token (S.Seq a) = a
  type Tokens (S.Seq a) = S.Seq a
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = S.fromList
  chunkToTokens Proxy = toList
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ S.Empty = Nothing
  take1_ (t S.:<| ts) = Just (t, ts)
  takeN_ n s
    | n <= 0 = Just (S.empty, s)
    | null s = Nothing
    | otherwise = Just (S.splitAt n s)
  takeWhile_ = S.spanl

-- | This wrapper selects the input-sharing 'Stream' implementation for
-- 'T.Text' ('TL.Text') and 'B.ByteString' ('BL.ByteString'). By input
-- sharing we mean that our parsers will use slices whenever possible to
-- avoid having to copy parts of the input. See also the documentation of
-- 'T.split'.
--
-- Note that using slices is in general faster than copying; on the other
-- hand it also has the potential for causing surprising memory leaks: if
-- any slice of the input survives in the output, holding on to the output
-- will force the entire input 'T.Text'/'B.ByteString' to stay in memory!
-- Even when using lazy 'TL.Text'/'BL.ByteString' we will hold on to whole
-- chunks at a time leading to to significantly worse memory residency in
-- some cases.
--
-- See 'NoShareInput' for a somewhat slower implementation that avoids this
-- memory leak scenario.
--
-- @since 9.3.0
newtype ShareInput a = ShareInput {unShareInput :: a}

instance Stream (ShareInput B.ByteString) where
  type Token (ShareInput B.ByteString) = Word8
  type Tokens (ShareInput B.ByteString) = B.ByteString
  tokenToChunk Proxy = B.singleton
  tokensToChunk Proxy = B.pack
  chunkToTokens Proxy = B.unpack
  chunkLength Proxy = B.length
  chunkEmpty Proxy = B.null
  take1_ (ShareInput s) = second ShareInput <$> B.uncons s
  takeN_ n (ShareInput s)
    | n <= 0 = Just (B.empty, ShareInput s)
    | B.null s = Nothing
    | otherwise = Just . second ShareInput $ B.splitAt n s
  takeWhile_ p (ShareInput s) = second ShareInput $ B.span p s

instance Stream (ShareInput BL.ByteString) where
  type Token (ShareInput BL.ByteString) = Word8
  type Tokens (ShareInput BL.ByteString) = BL.ByteString
  tokenToChunk Proxy = BL.singleton
  tokensToChunk Proxy = BL.pack
  chunkToTokens Proxy = BL.unpack
  chunkLength Proxy = fromIntegral . BL.length
  chunkEmpty Proxy = BL.null
  take1_ (ShareInput s) = second ShareInput <$> BL.uncons s
  takeN_ n (ShareInput s)
    | n <= 0 = Just (BL.empty, ShareInput s)
    | BL.null s = Nothing
    | otherwise = Just . second ShareInput $ BL.splitAt (fromIntegral n) s
  takeWhile_ p (ShareInput s) = second ShareInput $ BL.span p s

instance Stream (ShareInput T.Text) where
  type Token (ShareInput T.Text) = Char
  type Tokens (ShareInput T.Text) = T.Text
  tokenToChunk Proxy = T.singleton
  tokensToChunk Proxy = T.pack
  chunkToTokens Proxy = T.unpack
  chunkLength Proxy = T.length
  chunkEmpty Proxy = T.null
  take1_ (ShareInput s) = second ShareInput <$> T.uncons s
  takeN_ n (ShareInput s)
    | n <= 0 = Just (T.empty, ShareInput s)
    | T.null s = Nothing
    | otherwise = Just . second ShareInput $ T.splitAt n s
  takeWhile_ p (ShareInput s) = second ShareInput $ T.span p s

instance Stream (ShareInput TL.Text) where
  type Token (ShareInput TL.Text) = Char
  type Tokens (ShareInput TL.Text) = TL.Text
  tokenToChunk Proxy = TL.singleton
  tokensToChunk Proxy = TL.pack
  chunkToTokens Proxy = TL.unpack
  chunkLength Proxy = fromIntegral . TL.length
  chunkEmpty Proxy = TL.null
  take1_ (ShareInput s) = second ShareInput <$> TL.uncons s
  takeN_ n (ShareInput s)
    | n <= 0 = Just (TL.empty, ShareInput s)
    | TL.null s = Nothing
    | otherwise = Just . second ShareInput $ TL.splitAt (fromIntegral n) s
  takeWhile_ p (ShareInput s) = second ShareInput $ TL.span p s

-- | This wrapper selects the no-input-sharing 'Stream' implementation for
-- 'T.Text' ('TL.Text') and 'B.ByteString' ('BL.ByteString'). This means
-- that our parsers will create independent copies rather than using slices
-- of the input. See also the documentation of 'T.copy'.
--
-- More importantly, any parser output will be independent of the input, and
-- holding on to parts of the output will never prevent the input from being
-- garbage collected.
--
-- For maximum performance you might consider using 'ShareInput' instead,
-- but beware of its pitfalls!
--
-- @since 9.3.0
newtype NoShareInput a = NoShareInput {unNoShareInput :: a}

instance Stream (NoShareInput B.ByteString) where
  type Token (NoShareInput B.ByteString) = Word8
  type Tokens (NoShareInput B.ByteString) = B.ByteString
  tokenToChunk Proxy = B.singleton
  tokensToChunk Proxy = B.pack
  chunkToTokens Proxy = B.unpack
  chunkLength Proxy = B.length
  chunkEmpty Proxy = B.null
  take1_ (NoShareInput s) = second NoShareInput <$> B.uncons s
  takeN_ n (NoShareInput s)
    | n <= 0 = Just (B.empty, NoShareInput s)
    | B.null s = Nothing
    | otherwise =
        let (result, rest) = B.splitAt n s
            -- To avoid sharing the entire input we create a clean copy of the result.
            unSharedResult = B.copy result
         in Just (unSharedResult, NoShareInput rest)
  takeWhile_ p (NoShareInput s) =
    let (result, rest) = B.span p s
        -- Ditto.
        unSharedResult = B.copy result
     in (unSharedResult, NoShareInput rest)

instance Stream (NoShareInput BL.ByteString) where
  type Token (NoShareInput BL.ByteString) = Word8
  type Tokens (NoShareInput BL.ByteString) = BL.ByteString
  tokenToChunk Proxy = BL.singleton
  tokensToChunk Proxy = BL.pack
  chunkToTokens Proxy = BL.unpack
  chunkLength Proxy = fromIntegral . BL.length
  chunkEmpty Proxy = BL.null
  take1_ (NoShareInput s) = second NoShareInput <$> BL.uncons s
  takeN_ n (NoShareInput s)
    | n <= 0 = Just (BL.empty, NoShareInput s)
    | BL.null s = Nothing
    | otherwise =
        let (result, rest) = BL.splitAt (fromIntegral n) s
            -- To avoid sharing the entire input we create a clean copy of the result.
            unSharedResult = BL.copy result
         in Just (unSharedResult, NoShareInput rest)
  takeWhile_ p (NoShareInput s) =
    let (result, rest) = BL.span p s
        -- Ditto.
        unSharedResult = BL.copy result
     in (unSharedResult, NoShareInput rest)

instance Stream (NoShareInput T.Text) where
  type Token (NoShareInput T.Text) = Char
  type Tokens (NoShareInput T.Text) = T.Text
  tokenToChunk Proxy = T.singleton
  tokensToChunk Proxy = T.pack
  chunkToTokens Proxy = T.unpack
  chunkLength Proxy = T.length
  chunkEmpty Proxy = T.null
  take1_ (NoShareInput s) = second NoShareInput <$> T.uncons s
  takeN_ n (NoShareInput s)
    | n <= 0 = Just (T.empty, NoShareInput s)
    | T.null s = Nothing
    | otherwise =
        let (result, rest) = T.splitAt n s
            -- To avoid sharing the entire input we create a clean copy of the result.
            unSharedResult = T.copy result
         in Just (unSharedResult, NoShareInput rest)
  takeWhile_ p (NoShareInput s) =
    let (result, rest) = T.span p s
        unSharedResult = T.copy result
     in (unSharedResult, NoShareInput rest)

instance Stream (NoShareInput TL.Text) where
  type Token (NoShareInput TL.Text) = Char
  type Tokens (NoShareInput TL.Text) = TL.Text
  tokenToChunk Proxy = TL.singleton
  tokensToChunk Proxy = TL.pack
  chunkToTokens Proxy = TL.unpack
  chunkLength Proxy = fromIntegral . TL.length
  chunkEmpty Proxy = TL.null
  take1_ (NoShareInput s) = second NoShareInput <$> TL.uncons s
  takeN_ n (NoShareInput s)
    | n <= 0 = Just (TL.empty, NoShareInput s)
    | TL.null s = Nothing
    | otherwise =
        let (result, rest) = TL.splitAt (fromIntegral n) s
            -- To avoid sharing the entire input we create a clean copy of the result.
            unSharedResult = tlCopy result
         in Just (unSharedResult, NoShareInput rest)
  takeWhile_ p (NoShareInput s) =
    let (result, rest) = TL.span p s
        unSharedResult = tlCopy result
     in (unSharedResult, NoShareInput rest)

-- | Create an independent copy of a TL.Text, akin to BL.copy.
tlCopy :: TL.Text -> TL.Text
tlCopy = TL.fromStrict . T.copy . TL.toStrict
{-# INLINE tlCopy #-}

-- Since we are using @{-# LANGUAGE Safe #-}@ we can't use deriving via in
-- these cases.

instance Stream B.ByteString where
  type Token B.ByteString = Token (ShareInput B.ByteString)
  type Tokens B.ByteString = Tokens (ShareInput B.ByteString)
  tokenToChunk Proxy = tokenToChunk (Proxy :: Proxy (ShareInput B.ByteString))
  tokensToChunk Proxy = tokensToChunk (Proxy :: Proxy (ShareInput B.ByteString))
  chunkToTokens Proxy = chunkToTokens (Proxy :: Proxy (ShareInput B.ByteString))
  chunkLength Proxy = chunkLength (Proxy :: Proxy (ShareInput B.ByteString))
  chunkEmpty Proxy = chunkEmpty (Proxy :: Proxy (ShareInput B.ByteString))
  take1_ s = second unShareInput <$> take1_ (ShareInput s)
  takeN_ n s = second unShareInput <$> takeN_ n (ShareInput s)
  takeWhile_ p s = second unShareInput $ takeWhile_ p (ShareInput s)

instance Stream BL.ByteString where
  type Token BL.ByteString = Token (ShareInput BL.ByteString)
  type Tokens BL.ByteString = Tokens (ShareInput BL.ByteString)
  tokenToChunk Proxy = tokenToChunk (Proxy :: Proxy (ShareInput BL.ByteString))
  tokensToChunk Proxy = tokensToChunk (Proxy :: Proxy (ShareInput BL.ByteString))
  chunkToTokens Proxy = chunkToTokens (Proxy :: Proxy (ShareInput BL.ByteString))
  chunkLength Proxy = chunkLength (Proxy :: Proxy (ShareInput BL.ByteString))
  chunkEmpty Proxy = chunkEmpty (Proxy :: Proxy (ShareInput BL.ByteString))
  take1_ s = second unShareInput <$> take1_ (ShareInput s)
  takeN_ n s = second unShareInput <$> takeN_ n (ShareInput s)
  takeWhile_ p s = second unShareInput $ takeWhile_ p (ShareInput s)

instance Stream T.Text where
  type Token T.Text = Token (ShareInput T.Text)
  type Tokens T.Text = Tokens (ShareInput T.Text)
  tokenToChunk Proxy = tokenToChunk (Proxy :: Proxy (ShareInput T.Text))
  tokensToChunk Proxy = tokensToChunk (Proxy :: Proxy (ShareInput T.Text))
  chunkToTokens Proxy = chunkToTokens (Proxy :: Proxy (ShareInput T.Text))
  chunkLength Proxy = chunkLength (Proxy :: Proxy (ShareInput T.Text))
  chunkEmpty Proxy = chunkEmpty (Proxy :: Proxy (ShareInput T.Text))
  take1_ s = second unShareInput <$> take1_ (ShareInput s)
  takeN_ n s = second unShareInput <$> takeN_ n (ShareInput s)
  takeWhile_ p s = second unShareInput $ takeWhile_ p (ShareInput s)

instance Stream TL.Text where
  type Token TL.Text = Token (ShareInput TL.Text)
  type Tokens TL.Text = Tokens (ShareInput TL.Text)
  tokenToChunk Proxy = tokenToChunk (Proxy :: Proxy (ShareInput TL.Text))
  tokensToChunk Proxy = tokensToChunk (Proxy :: Proxy (ShareInput TL.Text))
  chunkToTokens Proxy = chunkToTokens (Proxy :: Proxy (ShareInput TL.Text))
  chunkLength Proxy = chunkLength (Proxy :: Proxy (ShareInput TL.Text))
  chunkEmpty Proxy = chunkEmpty (Proxy :: Proxy (ShareInput TL.Text))
  take1_ s = second unShareInput <$> take1_ (ShareInput s)
  takeN_ n s = second unShareInput <$> takeN_ n (ShareInput s)
  takeWhile_ p s = second unShareInput $ takeWhile_ p (ShareInput s)

-- | Type class for inputs that can also be used for debugging.
--
-- @since 9.0.0
class (Stream s) => VisualStream s where
  -- | Pretty-print non-empty stream of tokens. This function is also used
  -- to print single tokens (represented as singleton lists).
  --
  -- @since 7.0.0
  showTokens :: Proxy s -> NonEmpty (Token s) -> String

  -- | Return the number of characters that a non-empty stream of tokens
  -- spans. The default implementation is sufficient if every token spans
  -- exactly 1 character.
  --
  -- @since 8.0.0
  tokensLength :: Proxy s -> NonEmpty (Token s) -> Int
  tokensLength Proxy = NE.length

instance VisualStream String where
  showTokens Proxy = stringPretty
  tokensLength Proxy = Unicode.stringLength

instance VisualStream B.ByteString where
  showTokens Proxy = stringPretty . fmap (chr . fromIntegral)

instance VisualStream BL.ByteString where
  showTokens Proxy = stringPretty . fmap (chr . fromIntegral)

instance VisualStream T.Text where
  showTokens Proxy = stringPretty
  tokensLength Proxy = Unicode.stringLength

instance VisualStream TL.Text where
  showTokens Proxy = stringPretty
  tokensLength Proxy = Unicode.stringLength

-- | Type class for inputs that can also be used for error reporting.
--
-- @since 9.0.0
class (Stream s) => TraversableStream s where
  {-# MINIMAL reachOffset | reachOffsetNoLine #-}

  -- | Given an offset @o@ and initial 'PosState', adjust the state in such
  -- a way that it starts at the offset.
  --
  -- Return two values (in order):
  --
  --     * 'Maybe' 'String' representing the line on which the given offset
  --       @o@ is located. It can be omitted (i.e. 'Nothing'); in that case
  --       error reporting functions will not show offending lines. If
  --       returned, the line should satisfy a number of conditions that are
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
  -- __Note__: type signature of the function was changed in the version
  -- /9.0.0/.
  --
  -- @since 7.0.0
  reachOffset ::
    -- | Offset to reach
    Int ->
    -- | Initial 'PosState' to use
    PosState s ->
    -- | See the description of the function
    (Maybe String, PosState s)
  reachOffset o pst =
    (Nothing, reachOffsetNoLine o pst)

  -- | A version of 'reachOffset' that may be faster because it doesn't need
  -- to fetch the line at which the given offset in located.
  --
  -- The default implementation is this:
  --
  -- > reachOffsetNoLine o pst =
  -- >   snd (reachOffset o pst)
  --
  -- __Note__: type signature of the function was changed in the version
  -- /8.0.0/.
  --
  -- @since 7.0.0
  reachOffsetNoLine ::
    -- | Offset to reach
    Int ->
    -- | Initial 'PosState' to use
    PosState s ->
    -- | Reached source position and updated state
    PosState s
  reachOffsetNoLine o pst =
    snd (reachOffset o pst)

instance TraversableStream String where
  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset o pst =
    reachOffset' splitAt foldl' id id ('\n', '\t') charInc o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' splitAt foldl' ('\n', '\t') charInc o pst

instance TraversableStream B.ByteString where
  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset o pst =
    reachOffset' B.splitAt B.foldl' B8.unpack (chr . fromIntegral) (10, 9) byteInc o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' B.splitAt B.foldl' (10, 9) byteInc o pst

instance TraversableStream BL.ByteString where
  -- NOTE Do not eta-reduce these (breaks inlining)
  reachOffset o pst =
    reachOffset' splitAtBL BL.foldl' BL8.unpack (chr . fromIntegral) (10, 9) byteInc o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' splitAtBL BL.foldl' (10, 9) byteInc o pst

instance TraversableStream T.Text where
  -- NOTE Do not eta-reduce (breaks inlining of reachOffset').
  reachOffset o pst =
    reachOffset' T.splitAt T.foldl' T.unpack id ('\n', '\t') charInc o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' T.splitAt T.foldl' ('\n', '\t') charInc o pst

instance TraversableStream TL.Text where
  -- NOTE Do not eta-reduce (breaks inlining of reachOffset').
  reachOffset o pst =
    reachOffset' splitAtTL TL.foldl' TL.unpack id ('\n', '\t') charInc o pst
  reachOffsetNoLine o pst =
    reachOffsetNoLine' splitAtTL TL.foldl' ('\n', '\t') charInc o pst

----------------------------------------------------------------------------
-- Helpers

-- | An internal helper state type combining a difference 'String' and an
-- unboxed 'SourcePos'.
data St = St {-# UNPACK #-} !SourcePos ShowS

-- | A helper definition to facilitate defining 'reachOffset' for various
-- stream types.
reachOffset' ::
  forall s.
  (Stream s) =>
  -- | How to split input stream at given offset
  (Int -> s -> (Tokens s, s)) ->
  -- | How to fold over input stream
  (forall b. (b -> Token s -> b) -> b -> Tokens s -> b) ->
  -- | How to convert chunk of input stream into a 'String'
  (Tokens s -> String) ->
  -- | How to convert a token into a 'Char'
  (Token s -> Char) ->
  -- | Newline token and tab token
  (Token s, Token s) ->
  -- | Increment in column position for a token
  (Token s -> Pos) ->
  -- | Offset to reach
  Int ->
  -- | Initial 'PosState' to use
  PosState s ->
  -- | Line at which 'SourcePos' is located, updated 'PosState'
  (Maybe String, PosState s)
reachOffset'
  splitAt'
  foldl''
  fromToks
  fromTok
  (newlineTok, tabTok)
  columnIncrement
  o
  PosState {..} =
    ( Just $ case expandTab pstateTabWidth
        . addPrefix
        . f
        . fromToks
        . fst
        $ takeWhile_ (/= newlineTok) post of
        "" -> "<empty line>"
        xs -> xs,
      PosState
        { pstateInput = post,
          pstateOffset = max pstateOffset o,
          pstateSourcePos = spos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix =
            if sameLine
              then -- NOTE We don't use difference lists here because it's
              -- desirable for 'PosState' to be an instance of 'Eq' and
              -- 'Show'. So we just do appending here. Fortunately several
              -- parse errors on the same line should be relatively rare.
                pstateLinePrefix ++ f ""
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
            w = unPos pstateTabWidth
         in if
              | ch == newlineTok ->
                  St
                    (SourcePos n (l <> pos1) pos1)
                    id
              | ch == tabTok ->
                  St
                    (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
                    (g . (fromTok ch :))
              | otherwise ->
                  St
                    (SourcePos n l (c <> columnIncrement ch))
                    (g . (fromTok ch :))
{-# INLINE reachOffset' #-}

-- | Like 'reachOffset'' but for 'reachOffsetNoLine'.
reachOffsetNoLine' ::
  forall s.
  (Stream s) =>
  -- | How to split input stream at given offset
  (Int -> s -> (Tokens s, s)) ->
  -- | How to fold over input stream
  (forall b. (b -> Token s -> b) -> b -> Tokens s -> b) ->
  -- | Newline token and tab token
  (Token s, Token s) ->
  -- | Offset to reach
  -- | Increment in column position for a token
  (Token s -> Pos) ->
  Int ->
  -- | Initial 'PosState' to use
  PosState s ->
  -- | Updated 'PosState'
  PosState s
reachOffsetNoLine'
  splitAt'
  foldl''
  (newlineTok, tabTok)
  columnIncrement
  o
  PosState {..} =
    ( PosState
        { pstateInput = post,
          pstateOffset = max pstateOffset o,
          pstateSourcePos = spos,
          pstateTabWidth = pstateTabWidth,
          pstateLinePrefix = pstateLinePrefix
        }
    )
    where
      spos = foldl'' go pstateSourcePos pre
      (pre, post) = splitAt' (o - pstateOffset) pstateInput
      go (SourcePos n l c) ch =
        let c' = unPos c
            w = unPos pstateTabWidth
         in if
              | ch == newlineTok ->
                  SourcePos n (l <> pos1) pos1
              | ch == tabTok ->
                  SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w))
              | otherwise ->
                  SourcePos n l (c <> columnIncrement ch)
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
stringPretty (x :| []) = charPretty x
stringPretty ('\r' :| "\n") = "crlf newline"
stringPretty xs = "\"" <> concatMap f (NE.toList xs) <> "\""
  where
    f ch =
      case charPretty' ch of
        Nothing -> [ch]
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
  '\BS' -> Just "backspace"
  '\t' -> Just "tab"
  '\n' -> Just "newline"
  '\v' -> Just "vertical tab"
  '\f' -> Just "form feed"
  '\r' -> Just "carriage return"
  '\SO' -> Just "shift out"
  '\SI' -> Just "shift in"
  '\DLE' -> Just "data link escape"
  '\DC1' -> Just "device control one"
  '\DC2' -> Just "device control two"
  '\DC3' -> Just "device control three"
  '\DC4' -> Just "device control four"
  '\NAK' -> Just "negative acknowledge"
  '\SYN' -> Just "synchronous idle"
  '\ETB' -> Just "end of transmission block"
  '\CAN' -> Just "cancel"
  '\EM' -> Just "end of medium"
  '\SUB' -> Just "substitute"
  '\ESC' -> Just "escape"
  '\FS' -> Just "file separator"
  '\GS' -> Just "group separator"
  '\RS' -> Just "record separator"
  '\US' -> Just "unit separator"
  '\DEL' -> Just "delete"
  '\160' -> Just "non-breaking space"
  _ -> Nothing

-- | Replace tab characters with given number of spaces.
expandTab ::
  Pos ->
  String ->
  String
expandTab w' = go 0 0
  where
    go _ 0 [] = []
    go !i 0 ('\t' : xs) = go i (w - (i `rem` w)) xs
    go !i 0 (x : xs) = x : go (i + 1) 0 xs
    go !i n xs = ' ' : go (i + 1) (n - 1) xs
    w = unPos w'

-- | Return increment in column position that corresponds to the given
-- 'Char'.
charInc :: Char -> Pos
charInc ch = if Unicode.isWideChar ch then pos1 <> pos1 else pos1

-- | Return increment in column position that corresponds to the given
-- 'Word8'.
byteInc :: Word8 -> Pos
byteInc _ = pos1
