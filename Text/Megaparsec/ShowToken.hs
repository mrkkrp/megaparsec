-- |
-- Module      :  Text.Megaparsec.ShowToken
-- Copyright   :  © 2015–2016 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Pretty printing function and instances for use in error messages.

module Text.Megaparsec.ShowToken (ShowToken (..)) where

-- | Typeclass 'ShowToken' defines single function 'showToken' that can be
-- used to “pretty-print” various tokens. By default, all commonly used
-- instances are defined, but you can add your own, of course.

class Show a => ShowToken a where

  -- | Pretty-print given token. This is used to get token representation
  -- for use in error messages.

  showToken :: a -> String

instance ShowToken Char where
  showToken = prettyChar

-- | @prettyChar ch@ returns user-friendly string representation of given
-- character @ch@, suitable for using in error messages, for example.

prettyChar :: Char -> String
prettyChar '\0' = "null"
prettyChar '\a' = "bell"
prettyChar '\b' = "backspace"
prettyChar '\t' = "tab"
prettyChar '\n' = "newline"
prettyChar '\v' = "vertical tab"
prettyChar '\f' = "form feed"
prettyChar '\r' = "carriage return"
prettyChar ' '  = "space"
prettyChar x    = "'" ++ [x] ++ "'"

instance ShowToken String where
  showToken = prettyString

-- | @prettyString s@ returns pretty representation of string @s@. This is
-- used when printing string tokens in error messages.

prettyString :: String -> String
prettyString ""     = ""
prettyString [x]    = prettyChar x
prettyString "\r\n" = "crlf newline"
prettyString xs     = "\"" ++ xs ++ "\""
