-- |
-- Module      :  Text.MegaParsec.Language
-- Copyright   :  © 2015 MegaParsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable (uses non-portable module Text.Parsec.Token)
--
-- A helper module that defines some language definitions that can be used
-- to instantiate a token parser (see "Text.Parsec.Token").

module Text.MegaParsec.Language
    ( LanguageDef
    , emptyDef
    , haskellStyle
    , javaStyle
    , haskellDef
    , mondrianDef )
where

import Control.Monad.Identity

import Text.MegaParsec
import Text.MegaParsec.Token

-- | This is the most minimal token definition. It is recommended to use
-- this definition as the basis for other definitions. @emptyDef@ has no
-- reserved names or operators, is case sensitive and doesn't accept
-- comments, identifiers or operators.

emptyDef :: LanguageDef String st Identity
emptyDef =
    LanguageDef
    { commentStart    = ""
    , commentEnd      = ""
    , commentLine     = ""
    , nestedComments  = True
    , identStart      = letter <|> char '_'
    , identLetter     = alphaNum <|> oneOf "_'"
    , opStart         = opLetter emptyDef
    , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames = []
    , reservedNames   = []
    , caseSensitive   = True }

-- | This is a minimal token definition for Haskell style languages. It
-- defines the style of comments, valid identifiers and case sensitivity. It
-- does not define any reserved words or operators.

haskellStyle :: LanguageDef String u Identity
haskellStyle =
    emptyDef
    { commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , nestedComments  = True
    , identStart      = letter
    , identLetter     = alphaNum <|> oneOf "_'"
    , opStart         = opLetter haskellStyle
    , opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , reservedOpNames = []
    , reservedNames   = []
    , caseSensitive   = True }

-- | This is a minimal token definition for Java style languages. It
-- defines the style of comments, valid identifiers and case sensitivity. It
-- does not define any reserved words or operators.

javaStyle  :: LanguageDef String u Identity
javaStyle =
    emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , nestedComments  = True
    , identStart      = letter
    , identLetter     = alphaNum <|> oneOf "_'"
    , reservedNames   = []
    , reservedOpNames = []
    , caseSensitive   = False }

-- | The language definition for the Haskell language.

haskellDef  :: LanguageDef String u Identity
haskellDef =
    haskell98Def
    { identLetter   = identLetter haskell98Def <|> char '#'
    , reservedNames = reservedNames haskell98Def ++
                       [ "foreign", "import", "export", "primitive"
                       , "_ccall_", "_casm_", "forall"] }

-- | The language definition for the language Haskell98.

haskell98Def :: LanguageDef String u Identity
haskell98Def =
    haskellStyle
    { reservedOpNames = ["::","..","=","\\","|","<-","->","@","~","=>"]
    , reservedNames   = [ "let", "in", "case", "of", "if", "then", "else"
                        , "data", "type", "class", "default", "deriving"
                        , "do", "import", "infix", "infixl", "infixr"
                        , "instance", "module", "newtype", "where"
                        , "primitive" ] }

-- | The language definition for the language Mondrian.

mondrianDef :: LanguageDef String u Identity
mondrianDef =
    javaStyle
    { reservedNames = [ "case", "class", "default", "extends"
                      , "import", "in", "let", "new", "of", "package" ]
    , caseSensitive = True }
