{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Text.Megaparsec.Byte.Binary
-- Copyright   :  © 2021–present Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Binary-format number parsers.
--
-- @since 9.2.0
module Text.Megaparsec.Byte.Binary
  ( -- * Generic parsers
    BinaryChunk (..),
    anyLE,
    anyBE,

    -- * Parsing unsigned values
    word8,
    word16le,
    word16be,
    word32le,
    word32be,
    word64le,
    word64be,

    -- * Parsing signed values
    int8,
    int16le,
    int16be,
    int32le,
    int32be,
    int64le,
    int64be,
  )
where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Int
import Data.Word
import Text.Megaparsec

-- | Data types that can be converted to little- or big- endian numbers.
class BinaryChunk chunk where
  convertChunkBE :: (Bits a, Num a) => chunk -> a
  convertChunkLE :: (Bits a, Num a) => chunk -> a

instance BinaryChunk B.ByteString where
  convertChunkBE = B.foldl' go 0
    where
      go acc byte = (acc `unsafeShiftL` 8) .|. fromIntegral byte
  convertChunkLE = B.foldl' go 0
    where
      go acc byte = (acc .|. fromIntegral byte) `rotateR` 8

instance BinaryChunk BL.ByteString where
  convertChunkBE = BL.foldl' go 0
    where
      go acc byte = (acc `unsafeShiftL` 8) .|. fromIntegral byte
  convertChunkLE = BL.foldl' go 0
    where
      go acc byte = (acc .|. fromIntegral byte) `rotateR` 8

----------------------------------------------------------------------------
-- Generic parsers

-- | Parse a little-endian number.
--
-- You may wish to call this with a visible type application:
--
-- > number <- anyLE (Just "little-endian 32 bit word") @Word32
anyLE ::
  forall a e s m.
  (MonadParsec e s m, FiniteBits a, Num a, BinaryChunk (Tokens s)) =>
  -- | Label, if any
  Maybe String ->
  m a
anyLE mlabel = convertChunkLE <$> takeP mlabel (finiteByteSize @a)
{-# INLINE anyLE #-}

-- | Parse a big-endian number.
--
-- You may wish to call this with a visible type application:
--
-- > number <- anyBE (Just "big-endian 32 bit word") @Word32
anyBE ::
  forall a e s m.
  (MonadParsec e s m, FiniteBits a, Num a, BinaryChunk (Tokens s)) =>
  -- | Label, if any
  Maybe String ->
  m a
anyBE mlabel = convertChunkBE <$> takeP mlabel (finiteByteSize @a)
{-# INLINE anyBE #-}

--------------------------------------------------------------------------------
-- Parsing unsigned values

-- | Parse a 'Word8'.
word8 :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Word8
word8 = anyBE (Just "8 bit word")
{-# INLINE word8 #-}

-- | Parse a little-endian 'Word16'.
word16le :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Word16
word16le = anyLE (Just "little-endian 16 bit word")
{-# INLINE word16le #-}

-- | Parse a big-endian 'Word16'.
word16be :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Word16
word16be = anyBE (Just "big-endian 16 bit word")
{-# INLINE word16be #-}

-- | Parse a little-endian 'Word32'.
word32le :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Word32
word32le = anyLE (Just "little-endian 32 bit word")
{-# INLINE word32le #-}

-- | Parse a big-endian 'Word32'.
word32be :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Word32
word32be = anyBE (Just "big-endian 32 bit word")
{-# INLINE word32be #-}

-- | Parse a little-endian 'Word64'.
word64le :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Word64
word64le = anyLE (Just "little-endian 64 word")
{-# INLINE word64le #-}

-- | Parse a big-endian 'Word64'.
word64be :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Word64
word64be = anyBE (Just "big-endian 64 word")
{-# INLINE word64be #-}

----------------------------------------------------------------------------
-- Parsing signed values

-- | Parse a 'Int8'.
int8 :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Int8
int8 = anyBE (Just "8 bit int")
{-# INLINE int8 #-}

-- | Parse a little-endian 'Int16'.
int16le :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Int16
int16le = anyLE (Just "little-endian 16 bit int")
{-# INLINE int16le #-}

-- | Parse a big-endian 'Int16'.
int16be :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Int16
int16be = anyBE (Just "big-endian 16 bit int")
{-# INLINE int16be #-}

-- | Parse a little-endian 'Int32'.
int32le :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Int32
int32le = anyLE (Just "little-endian 32 bit int")
{-# INLINE int32le #-}

-- | Parse a big-endian 'Int32'.
int32be :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Int32
int32be = anyBE (Just "big-endian 32 bit int")
{-# INLINE int32be #-}

-- | Parse a little-endian 'Int64'.
int64le :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Int64
int64le = anyLE (Just "little-endian 64 int")
{-# INLINE int64le #-}

-- | Parse a big-endian 'Int64'.
int64be :: (MonadParsec e s m, BinaryChunk (Tokens s)) => m Int64
int64be = anyBE (Just "big-endian 64 int")
{-# INLINE int64be #-}

--------------------------------------------------------------------------------
-- Helpers

-- | Return the number of bytes in the argument.
--
-- Performs ceiling division, so byte-unaligned types (bitsize not a
-- multiple of 8) should work, but further usage is not tested.
finiteByteSize :: forall a. (FiniteBits a) => Int
finiteByteSize = finiteBitSize @a undefined `ceilDiv` 8
  where
    ceilDiv x y = (x + y - 1) `div` y
{-# INLINE finiteByteSize #-}
