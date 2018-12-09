-- |
-- Module      :  Text.Megaparsec.Stream
-- Copyright   :  © 2015–2018 Megaparsec contributors
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

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Text.Megaparsec.Stream
  ( Stream (..), reachOffset, reachOffsetNoLine, takeN_, stringPretty )
where

import           Prelude               hiding (break, span, splitAt)

import qualified Data.ByteString       as B
import qualified Data.ByteString.Lazy  as BL
import           Data.Char             (chr)
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.List.NonEmpty    as NE
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import           Text.Megaparsec.Pos
import           Text.Megaparsec.State

import           Data.MonoTraversable
import           Data.Sequences

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif


class (IsSequence (Tokens s), Ord (Tokens s), Ord (Element (Tokens s))) => Stream s where
  type Tokens s :: *
  isTab :: Element (Tokens s) -> Bool
  isTab = const False
  isLF :: Element (Tokens s) -> Bool
  isLF = const False

  toStr :: Element (Tokens s) -> String
  default toStr :: Show (Element (Tokens s)) => Element (Tokens s) -> String
  toStr = show

  prettyTokens :: NonEmpty (Element (Tokens s)) -> String
  default prettyTokens :: Show (Element (Tokens s)) => NonEmpty (Element (Tokens s)) -> String
  prettyTokens = show

instance Stream String where
  type Tokens String = String
  isTab = (=='\t')
  isLF = (=='\n')
  toStr = (:[])
  prettyTokens = stringPretty

instance Stream B.ByteString where
  type Tokens B.ByteString = B.ByteString
  isTab = (==9)
  isLF = (==10)
  toStr x = [chr (fromIntegral x)]
  prettyTokens s = stringPretty $ NE.map (chr . fromIntegral) s

instance Stream BL.ByteString where
  type Tokens BL.ByteString = BL.ByteString
  isTab = (==9)
  isLF = (==10)
  toStr x = [chr (fromIntegral x)]
  prettyTokens s = stringPretty $ NE.map (chr . fromIntegral) s

instance Stream T.Text where
  type Tokens T.Text = T.Text
  isTab = (=='\t')
  isLF = (=='\n')
  toStr = (:[])
  prettyTokens = stringPretty

instance Stream TL.Text where
  type Tokens TL.Text = TL.Text
  isTab = (=='\t')
  isLF = (=='\n')
  toStr = (:[])
  prettyTokens = stringPretty


takeN_ :: Stream s => Int -> Tokens s -> Maybe (Tokens s, Tokens s)
takeN_ n s
  | n <= 0    = Just (mempty, s)
  | onull s    = Nothing
  | otherwise = Just (splitAt (fromIntegral n) s)

----------------------------------------------------------------------------
-- Helpers

-- | An internal helper state type combining a difference 'String' and an
-- unboxed 'SourcePos'.

data St = St SourcePos ShowS

-- {-# UNPACK #-} -- TODO do we need to unpack or not?


showTokens :: forall s . Stream s
           => Tokens s -> String
showTokens = concatMap (toStr @s) . otoList

-- reachOffset Int -> PosState a -> (SourcePos, String, PosState)
-- reachOffset' B.splitAt B.foldl' B8.unpack (chr . fromIntegral) (10, 9) o pst

reachOffset :: forall s . Stream s
             => Int
             -> PosState (Tokens s)
             -> (SourcePos, String, PosState (Tokens s))
reachOffset o PosState {..} =
  ( spos
  , case expandTab pstateTabWidth
           . addPrefix
           . f
           . showTokens @s
           . fst
           $ span (not . isLF @s) post of
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
    (pre, post) = splitAt (fromIntegral $ o - pstateOffset) pstateInput
    St spos f = ofoldl' go (St pstateSourcePos id) pre
    go (St apos g) ch =
      let SourcePos n l c = apos
          c' = unPos c
          w  = unPos pstateTabWidth
      in if | isLF @s ch ->
                St (SourcePos n (l <> pos1) pos1)
                   id
            | isTab @s ch ->
                St (SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w)))
                   (g . (toStr @s ch ++))
            | otherwise ->
                St (SourcePos n l (c <> pos1))
                   (g . (toStr @s ch ++))


-- | Like 'reachOffset'' but for 'reachOffsetNoLine'.

reachOffsetNoLine :: forall s . Stream s
             => Int
             -> PosState (Tokens s)
             -> (SourcePos, PosState (Tokens s))
reachOffsetNoLine o PosState {..} =
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
    spos = ofoldl' go pstateSourcePos pre
    (pre, post) = splitAt (fromIntegral $ o - pstateOffset) pstateInput
    go (SourcePos n l c) ch =
      let c' = unPos c
          w  = unPos pstateTabWidth
      in if | isLF @s ch ->
                SourcePos n (l <> pos1) pos1
            | isTab @s ch->
                SourcePos n l (mkPos $ c' + w - ((c' - 1) `rem` w))
            | otherwise ->
                SourcePos n l (c <> pos1)



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
charPretty ch  = fromMaybe ("'" <> [ch] <> "'") (charPretty' ch)

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
