-- |
-- Module      :  Text.Megaparsec.Lexer
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable (uses local universal quantification: PolymorphicComponents)
--
-- A helper module to parse lexical elements. See 'makeLexer' for a
-- description of how to use it. This module is supposed to be imported
-- qualified.

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Text.Megaparsec.Lexer
  ( LanguageDef (..)
  , defaultLang
  , skipLineComment
  , skipBlockComment
  , Lexer (..)
  , makeLexer )
where

import Control.Applicative ((<|>), many, some, empty)
import Control.Monad (void)
import Data.Char (isAlpha, toLower, toUpper)
import Data.List (sort)

import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Char as C

-- Language definition

-- | The @LanguageDef@ type is a record that contains all parameters used to
-- control features of the "Text.Megaparsec.Lexer" module. 'defaultLang' can
-- be used as a basis for new language definitions.

data LanguageDef s u m =
  LanguageDef {

  -- | The parser is used to parse single white space character. If
  -- indentation is important in your language you should probably not treat
  -- newline as white space character. Also note that if newline is not
  -- white space character, you will need to pick it up manually.

    spaceChar :: ParsecT s u m Char

  -- | The parser parses line comments. It's responsibility of the parser to
  -- stop at the end of line. If your language doesn't support this type of
  -- comments, set this value to 'empty'. In simple cases you can use
  -- 'skipLineComment' to quickly construct line comment parser.

  , lineComment :: ParsecT s u m ()

  -- | The parser parses block (multi-line) comments. If your language
  -- doesn't support this type of comments, set this value to 'empty'. In
  -- simple cases you can use 'skipBlockComment' to quickly construct block
  -- comment parser.

  , blockComment :: ParsecT s u m ()

  -- NEXT

  -- | This parser should accept any start characters of identifiers, for
  -- example @letter \<|> char \'_\'@.

  , identStart :: ParsecT s u m Char

  -- | This parser should accept any legal tail characters of identifiers,
  -- for example @alphaNum \<|> char \'_\'@.

  , identLetter :: ParsecT s u m Char

  -- | This parser should accept any start characters of operators, for
  -- example @oneOf \":!#$%&*+.\/\<=>?\@\\\\^|-~\"@

  , opStart :: ParsecT s u m Char

  -- | This parser should accept any legal tail characters of operators.
  -- Note that this parser should even be defined if the language doesn't
  -- support user-defined operators, or otherwise the 'reservedOp' parser
  -- won't work correctly.

  , opLetter :: ParsecT s u m Char

  -- | The list of reserved identifiers.

  , reservedNames :: [String]

  -- | The list of reserved operators.

  , reservedOpNames :: [String]

  -- | Set to 'True' if the language is case sensitive.

  , caseSensitive :: Bool }

-- Default language definition

-- | This is standard language definition. It is recommended to use
-- this definition as the basis for other definitions. @defaultLang@ has no
-- reserved names or operators, is case sensitive and doesn't accept
-- comments, identifiers or operators.

defaultLang :: Stream s m Char => LanguageDef s u m
defaultLang =
  LanguageDef
  { spaceChar       = C.spaceChar
  , lineComment     = empty
  , blockComment    = empty
  -- NEXT
  , identStart      = C.letterChar <|> C.char '_'
  , identLetter     = C.alphaNumChar <|> C.oneOf "_'"
  , opStart         = opLetter defaultLang
  , opLetter        = C.oneOf ":!#$%&*+./<=>?@\\^|-~"
  , reservedOpNames = []
  , reservedNames   = []
  , caseSensitive   = True }

-- Utility functions

-- | Given comment prefix this function returns parser that skips line
-- comments. Note that it stops just before newline character but doesn't
-- consume the newline. Newline is either supposed to be consumed by 'space'
-- parser or picked manually.

skipLineComment :: Stream s m Char => String -> ParsecT s u m ()
skipLineComment prefix = C.string prefix >> void (manyTill C.anyChar n)
  where n = lookAhead C.newline

-- | @skipBlockComment start end@ skips non-nested block comment starting
-- with @start@ and ending with @end@.

skipBlockComment :: Stream s m Char => String -> String -> ParsecT s u m ()
skipBlockComment start end = C.string start >> void (manyTill C.anyChar n)
  where n = lookAhead (C.string end)

-- Lexer

-- | The type of the record that holds lexical parsers that work on
-- @s@ streams with state @u@ over a monad @m@.

data Lexer s u m =
  Lexer {

  -- | Skips any white space. White space consists of /zero/ or more
  -- occurrences of 'spaceChar', a line comment or a block (multi-line)
  -- comment.

    space :: ParsecT s u m ()

  -- | @lexeme p@ first applies parser @p@ and then the 'space' parser,
  -- returning the value of @p@. Every lexical token (lexeme) is defined
  -- using @lexeme@, this way every parse starts at a point without white
  -- space. Parsers that use @lexeme@ are called /lexeme/ parsers in this
  -- document.
  --
  -- The only point where the 'space' parser should be called explicitly is
  -- the start of the main parser in order to skip any leading white space.

  , lexeme :: forall a. ParsecT s u m a -> ParsecT s u m a

  -- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
  -- trailing white space.

  , symbol :: String -> ParsecT s u m String

  -- | @indentGuard p@ consumes all white space it can consume, then checks
  -- column number. The column number should satisfy given predicate @p@,
  -- otherwise the parser fails with “incorrect indentation” message. In
  -- successful cases @indentGuard@ returns current column number.

  , indentGuard :: (Int -> Bool) -> ParsecT s u m Int

  -- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
  -- returning the value of @p@.

  , parens :: forall a. ParsecT s u m a -> ParsecT s u m a

  -- | Lexeme parser @braces p@ parses @p@ enclosed in braces (“{” and
  -- “}”), returning the value of @p@.

  , braces :: forall a. ParsecT s u m a -> ParsecT s u m a

  -- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (“\<”
  -- and “>”), returning the value of @p@.

  , angles :: forall a. ParsecT s u m a -> ParsecT s u m a

  -- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (“[”
  -- and “]”), returning the value of @p@.

  , brackets :: forall a. ParsecT s u m a -> ParsecT s u m a

  -- | Lexeme parser @semicolon@ parses the character “;” and skips any
  -- trailing white space. Returns the string “;”.

  , semicolon :: ParsecT s u m String

  -- | Lexeme parser @comma@ parses the character “,” and skips any
  -- trailing white space. Returns the string “,”.

  , comma :: ParsecT s u m String

  -- | Lexeme parser @colon@ parses the character “:” and skips any
  -- trailing white space. Returns the string “:”.

  , colon :: ParsecT s u m String

  -- | Lexeme parser @dot@ parses the character “.” and skips any
  -- trailing white space. Returns the string “.”.

  , dot :: ParsecT s u m String

  -- | The lexeme parser parses a single literal character. Returns the
  -- literal character value. This parsers deals correctly with escape
  -- sequences. The literal character is parsed according to the grammar
  -- rules defined in the Haskell report (which matches most programming
  -- languages quite closely).

  , charLiteral :: ParsecT s u m Char

  -- | The lexeme parser parses a literal string. Returns the literal
  -- string value. This parsers deals correctly with escape sequences and
  -- gaps. The literal string is parsed according to the grammar rules
  -- defined in the Haskell report (which matches most programming languages
  -- quite closely).

  , stringLiteral :: ParsecT s u m String

  -- | The lexeme parser parses an integer (a whole number). This parser
  -- /does not/ parse sign. Returns the value of the number. The number can
  -- be specified in 'decimal', 'hexadecimal' or 'octal'. The number is
  -- parsed according to the grammar rules in the Haskell report.

  , integer :: ParsecT s u m Integer

  -- | This is just like 'integer', except it can parse sign.

  , integer' :: ParsecT s u m Integer

  -- | The lexeme parses a positive whole number in the decimal system.
  -- Returns the value of the number.

  , decimal :: ParsecT s u m Integer

  -- | The lexeme parses a positive whole number in the hexadecimal
  -- system. The number should be prefixed with “0x” or “0X”. Returns the
  -- value of the number.

  , hexadecimal :: ParsecT s u m Integer

  -- | The lexeme parses a positive whole number in the octal system.
  -- The number should be prefixed with “0o” or “0O”. Returns the value of
  -- the number.

  , octal :: ParsecT s u m Integer

  -- | @signed p@ tries to parse sign (i.e. “+”, “-”, or nothing) and
  -- then runs parser @p@, changing sign of its result accordingly. Note
  -- that there may be white space after the sign but not before it.

  , signed :: forall a. Num a => ParsecT s u m a -> ParsecT s u m a

  -- | The lexeme parser parses a floating point value. Returns the value
  -- of the number. The number is parsed according to the grammar rules
  -- defined in the Haskell report, sign is /not/ parsed, use 'float'' to
  -- achieve parsing of signed floating point values.

  , float :: ParsecT s u m Double

  -- | This is just like 'float', except it can parse sign.

  , float' :: ParsecT s u m Double

  -- | The lexeme parser parses either 'integer' or a 'float'.
  -- Returns the value of the number. This parser deals with any overlap in
  -- the grammar rules for integers and floats. The number is parsed
  -- according to the grammar rules defined in the Haskell report.

  , number :: ParsecT s u m (Either Integer Double)

  -- | This is just like 'number', except it can parse sign.

  , number' :: ParsecT s u m (Either Integer Double)

  -- | The lexeme parser parses a legal identifier. Returns the identifier
  -- string. This parser will fail on identifiers that are reserved
  -- words. Legal identifier (start) characters and reserved words are
  -- defined in the 'LanguageDef' that is passed to 'makeLexer'.

  , identifier :: ParsecT s u m String

  -- | The lexeme parser @reserved name@ parses @symbol name@, but it also
  -- checks that the @name@ is not a prefix of a valid identifier.

  , reserved :: String -> ParsecT s u m ()

  -- | The lexeme parser parses a legal operator. Returns the name of the
  -- operator. This parser will fail on any operators that are reserved
  -- operators. Legal operator (start) characters and reserved operators are
  -- defined in the 'LanguageDef' that is passed to 'makeLexer'.

  , operator :: ParsecT s u m String

  -- | The lexeme parser @reservedOp name@ parses @symbol name@, but it
  -- also checks that the @name@ is not a prefix of a valid operator.

  , reservedOp :: String -> ParsecT s u m () }

-- | The expression @makeLexer language@ creates a 'Lexer' record that
-- contains lexical parsers that are defined using the definitions in the
-- @language@ record.
--
-- The use of this function is quite stylized — one imports the appropriate
-- language definition and selects the lexical parsers that are needed from
-- the resulting 'Lexer'.
--
-- > module Main (main) where
-- >
-- > import Text.Megaparsec
-- > import Text.Megaparsec.Language (haskellDef)
-- > import qualified Text.Megaparsec.Lexer as L
-- >
-- > -- The parser
-- > …
-- >
-- > expr =  parens expr
-- >     <|> identifier
-- >     <|> …
-- >
-- > -- The lexer
-- > lexer      = L.makeLexer haskellDef
-- >
-- > parens     = L.parens     lexer
-- > braces     = L.braces     lexer
-- > identifier = L.identifier lexer
-- > reserved   = L.reserved   lexer
-- > …

makeLexer :: Stream s m Char => LanguageDef s u m -> Lexer s u m
makeLexer lang =
  Lexer
  { space         = space
  , lexeme        = lexeme
  , symbol        = symbol
  , indentGuard   = indentGuard

  , parens        = parens
  , braces        = braces
  , angles        = angles
  , brackets      = brackets
  , semicolon     = semicolon
  , comma         = comma
  , colon         = colon
  , dot           = dot

  , charLiteral   = charLiteral
  , stringLiteral = stringLiteral

  , integer       = integer
  , integer'      = integer'
  , decimal       = decimal
  , hexadecimal   = hexadecimal
  , octal         = octal
  , signed        = signed
  , float         = float
  , float'        = float'
  , number        = number
  , number'       = number'

  , identifier    = identifier
  , reserved      = reserved
  , operator      = operator
  , reservedOp    = reservedOp }
  where

  -- white space & indentation

  space    = hidden . skipMany . choice $
             ($ lang) <$> [void . spaceChar, blockComment, lineComment]
  lexeme p = p <* space
  symbol   = lexeme . C.string
  indentGuard p = do
    space
    pos <- sourceColumn <$> getPosition
    if p pos
    then return pos
    else fail "incorrect indentation"

  -- auxiliary parsers

  parens    = between (symbol "(") (symbol ")")
  braces    = between (symbol "{") (symbol "}")
  angles    = between (symbol "<") (symbol ">")
  brackets  = between (symbol "[") (symbol "]")
  semicolon = symbol ";"
  comma     = symbol ","
  colon     = symbol ":"
  dot       = symbol "."

  -- char & string literals

  charLiteral = lexeme ( between (C.char '\'')
                                 (C.char '\'' <?> "end of character")
                                 characterChar )
                <?> "character"

  characterChar = charLetter <|> charEscape <?> "literal character"

  charEscape = C.char '\\' >> escapeCode
  charLetter = C.satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

  stringLiteral =
      lexeme ((foldr (maybe id (:)) "" <$>
               between (C.char '"') (C.char '"' <?> "end of string")
                           (many stringChar)) <?> "literal string")

  stringChar = (Just <$> stringLetter) <|> stringEscape <?> "string character"

  stringLetter = C.satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

  stringEscape = C.char '\\' >>
                 ( (escapeGap >> return Nothing)   <|>
                   (escapeEmpty >> return Nothing) <|>
                   (Just <$> escapeCode) )

  escapeEmpty = C.char '&'
  escapeGap   = some C.spaceChar >> C.char '\\' <?> "end of string gap"

  -- escape codes

  escapeCode = charEsc <|> charNum <|> charAscii <|> charControl
               <?> "escape code"

  charEsc = choice (parseEsc <$> escMap)
      where parseEsc (c, code) = C.char c >> return code

  charNum = toEnum . fromInteger <$>
            ( decimal <|>
             (C.char 'o' >> nump "0o" C.octDigitChar) <|>
             (C.char 'x' >> nump "0x" C.hexDigitChar) )

  charAscii = choice (parseAscii <$> asciiMap)
      where parseAscii (asc, code) = try (C.string asc >> return code)

  charControl = toEnum . subtract 64 . fromEnum <$> (C.char '^' >> C.upperChar)

  -- escape code tables

  escMap      = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"
  asciiMap    = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

  ascii2codes = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
                 "FS","GS","RS","US","SP"]
  ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
                 "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
                 "CAN","SUB","ESC","DEL"]

  ascii2 = "\b\t\n\v\f\r\SO\SI\EM\FS\GS\RS\US "
  ascii3 = "\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\DLE\DC1\DC2\DC3\DC4\NAK\SYN\ETB\CAN\SUB\ESC\DEL"

  -- numbers — integers

  integer  = decimal
  integer' = signed integer

  decimal     = lexeme (nump "" C.digitChar <?> "integer")
  hexadecimal = lexeme $ C.char '0' >> C.oneOf "xX" >> nump "0x" C.hexDigitChar
  octal       = lexeme $ C.char '0' >> C.oneOf "oO" >> nump "0o" C.octDigitChar

  nump prefix baseDigit = read . (prefix ++) <$> some baseDigit

  signed p = ($) <$> option id (lexeme sign) <*> p

  sign :: (Stream s m Char, Num a) => ParsecT s u m (a -> a)
  sign = (C.char '+' *> return id) <|> (C.char '-' *> return negate)

  -- numbers — floats

  float  = lexeme ffloat <?> "float"
  float' = signed float

  ffloat = read <$> ffloat'
    where
      ffloat' = do
        decimal <- fDec
        rest <- fraction <|> fExp
        return $ decimal ++ rest

  fraction = do
    void $ C.char '.'
    decimal <- fDec
    exp <- option "" fExp
    return $ '.' : decimal ++  exp

  fDec = some C.digitChar

  fExp = do
    expChar <- C.oneOf "eE"
    signStr <- option "" (pure <$> C.oneOf "+-")
    decimal <- fDec
    return $ expChar : signStr ++ decimal

  -- numbers — a more general case

  number  = (Right <$> try float)  <|> (Left <$> integer)  <?> "number"
  number' = (Right <$> try float') <|> (Left <$> integer') <?> "number"

  -- operators & reserved ops

  reservedOp name =
      lexeme $ try $ do
        void $ C.string name
        notFollowedBy (opLetter lang) <?> ("end of " ++ show name)

  operator =
      lexeme $ try $ do
        name <- oper
        if isReservedOp name
        then unexpected ("reserved operator " ++ show name)
        else return name

  oper = ((:) <$> opStart lang <*> many (opLetter lang))
         <?> "operator"

  isReservedOp = isReserved . sort $ reservedOpNames lang

  -- identifiers & reserved words

  reserved name =
      lexeme $ try $ do
        void $ caseString name
        notFollowedBy (identLetter lang) <?> ("end of " ++ show name)

  caseString name
      | caseSensitive lang = C.string name
      | otherwise                 = walk name >> return name
      where walk = foldr (\c -> ((caseChar c <?> show name) >>)) (return ())
            caseChar c
                | isAlpha c = C.char (toLower c) <|> C.char (toUpper c)
                | otherwise = C.char c

  identifier =
      lexeme $ try $ do
        name <- ident
        if isReservedName name
        then unexpected ("reserved word " ++ show name)
        else return name

  ident = ((:) <$> identStart lang <*> many (identLetter lang))
          <?> "identifier"

  isReservedName name = isReserved theReservedNames caseName
      where caseName
                | caseSensitive lang = name
                | otherwise                 = toLower <$> name

  isReserved names name = scan names
      where scan []     = False
            scan (r:rs) = case compare r name of
                            LT  -> scan rs
                            EQ  -> True
                            GT  -> False

  theReservedNames
      | caseSensitive lang = sort reserved
      | otherwise                 = sort . fmap (fmap toLower) $ reserved
      where reserved = reservedNames lang
