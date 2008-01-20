-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.ByteString.Lazy
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  derek.a.elkins@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Make lazy ByteStrings an instance of 'Stream' with 'Char' token type.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Text.Parsec.ByteString.Lazy
    ( Parser, GenParser, parseFromFile
    ) where

import Text.Parsec.Error
import Text.Parsec.Prim

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Lazy.Char8 as C

instance (Monad m) => Stream C.ByteString m Char where
    uncons lbs = case C.toChunks lbs of
                    []      -> return Nothing -- TODO: should be something better than toEnum . fromEnum
                    (x:xs)  -> return $ Just (toEnum . fromEnum $ B.unsafeHead x,
                                              if B.length x == 1 then C.fromChunks xs
                                                                 else C.fromChunks (B.unsafeTail x:xs))

type Parser a = Parsec C.ByteString () a
type GenParser t st a = Parsec C.ByteString st a

-- | @parseFromFile p filePath@ runs a lazy bytestring parser @p@ on the
-- input read from @filePath@ using 'ByteString.Lazy.Char8.readFile'. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- >  main    = do{ result <- parseFromFile numbers "digits.txt"
-- >              ; case result of
-- >                  Left err  -> print err
-- >                  Right xs  -> print (sum xs)
-- >              }
parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- C.readFile fname
         return (runP p () fname input)
