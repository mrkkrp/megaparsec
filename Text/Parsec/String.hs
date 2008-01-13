-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.String
-- Copyright   :  (c) Paolo Martini 2007
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
-- 
-- Maintainer  :  paolo@nemail.com
-- Stability   :  provisional
-- Portability :  portable
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Text.Parsec.String
    ( Parser, GenParser, parseFromFile
    ) where

import Text.Parsec.Error
import Text.Parsec.Prim

instance (Monad m) => Stream [tok] m tok where
    uncons []     = return $ Nothing
    uncons (t:ts) = return $ Just (t,ts)

type Parser a = Parsec String () a
type GenParser tok st a = Parsec [tok] st a

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname
    = do input <- readFile fname
         return (runP p () fname input)
