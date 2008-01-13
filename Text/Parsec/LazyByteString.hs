-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.LazyByteString
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  paolo@nemail.com
-- Stability   :  provisional
-- Portability :  portable
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Text.Parsec.LazyByteString
    ( Parser, GenParser, parseFromFile
    ) where

import Text.Parsec.Error
import Text.Parsec.Prim

import qualified Data.ByteString as B
import qualified Data.ByteString.Base as B
import qualified Data.ByteString.Lazy.Char8 as C

instance (Monad m) => Stream C.ByteString m Char where
    uncons (B.LPS [])     = return Nothing
    uncons (B.LPS (x:xs)) = return $ Just $
            (toEnum $ fromEnum $ B.unsafeHead x,
             if B.length x == 1 then B.LPS xs else B.LPS (B.unsafeTail x : xs))

type Parser a = Parsec C.ByteString () a
type GenParser t st a = Parsec C.ByteString st a

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- C.readFile fname
         return (runP p () fname input)
