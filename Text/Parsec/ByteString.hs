-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.ByteString
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  paolo@nemail.com
-- Stability   :  provisional
-- Portability :  portable
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Text.Parsec.ByteString
    ( Parser, GenParser, parseFromFile
    ) where

import Text.Parsec.Error
import Text.Parsec.Prim

import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Char8 as C

instance (Monad m) => Stream C.ByteString m Char where
    uncons s
        | C.null s  = return $ Nothing
        | otherwise = return $ Just $
                                  ( toEnum $ fromEnum $ B.unsafeHead s
                                  , B.unsafeTail s)

type Parser a = Parsec C.ByteString () a
type GenParser t st a = Parsec C.ByteString st a

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- C.readFile fname
         return (runP p () fname input)
