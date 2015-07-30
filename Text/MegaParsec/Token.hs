-- |
-- Module      :  Text.MegaParsec.Token
-- Copyright   :  © 2015 MegaParsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable (uses local universal quantification: PolymorphicComponents)
--
-- A helper module to parse lexical elements (tokens). See 'makeTokenParser'
-- for a description of how to use it.

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Text.MegaParsec.Token
    ( LanguageDef (..)
    , TokenParser (..)
    , makeTokenParser )
where

import Data.Char (isAlpha, toLower, toUpper, isSpace, digitToInt)
import Data.List (nub, sort)

import Control.Monad (void)

import Text.MegaParsec.Prim
import Text.MegaParsec.Char
import Text.MegaParsec.Combinator

-- Language definition

-- | The @LanguageDef@ type is a record that contains all parameterizable
-- features of the "Text.Parsec.Token" module. The module
-- "Text.Parsec.Language" contains some default definitions.

data LanguageDef s u m =
    LanguageDef {

    -- | Describes the start of a block comment. Use the empty string if the
    -- language doesn't support block comments. For example \"\/*\".

      commentStart :: String

    -- | Describes the end of a block comment. Use the empty string if the
    -- language doesn't support block comments. For example \"*\/\".

    , commentEnd :: String

    -- | Describes the start of a line comment. Use the empty string if the
    -- language doesn't support line comments. For example \"\/\/\".

    , commentLine :: String

    -- | Set to 'True' if the language supports nested block comments.

    , nestedComments :: Bool

    -- | This parser should accept any start characters of identifiers. For
    -- example @letter \<|> char \'_\'@.

    , identStart :: ParsecT s u m Char

    -- | This parser should accept any legal tail characters of identifiers.
    -- For example @alphaNum \<|> char \'_\'@.

    , identLetter :: ParsecT s u m Char

    -- | This parser should accept any start characters of operators. For
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

-- Token parser

-- | The type of the record that holds lexical parsers that work on
-- @s@ streams with state @u@ over a monad @m@.

data TokenParser s u m =
    TokenParser {

    -- | This lexeme parser parses a legal identifier. Returns the identifier
    -- string. This parser will fail on identifiers that are reserved
    -- words. Legal identifier (start) characters and reserved words are
    -- defined in the 'LanguageDef' that is passed to 'makeTokenParser'. An
    -- @identifier@ is treated as a single token using 'try'.

      identifier :: ParsecT s u m String

    -- | The lexeme parser @reserved name@ parses @symbol
    -- name@, but it also checks that the @name@ is not a prefix of a valid
    -- identifier. A @reserved@ word is treated as a single token using
    -- 'try'.

    , reserved :: String -> ParsecT s u m ()

    -- | This lexeme parser parses a legal operator. Returns the name of the
    -- operator. This parser will fail on any operators that are reserved
    -- operators. Legal operator (start) characters and reserved operators
    -- are defined in the 'LanguageDef' that is passed to
    -- 'makeTokenParser'. An @operator@ is treated as a single token using
    -- 'try'.

    , operator :: ParsecT s u m String

    -- |The lexeme parser @reservedOp name@ parses @symbol
    -- name@, but it also checks that the @name@ is not a prefix of a valid
    -- operator. A @reservedOp@ is treated as a single token using 'try'.

    , reservedOp :: String -> ParsecT s u m ()

    -- | This lexeme parser parses a single literal character. Returns the
    -- literal character value. This parsers deals correctly with escape
    -- sequences. The literal character is parsed according to the grammar
    -- rules defined in the Haskell report (which matches most programming
    -- languages quite closely).

    , charLiteral :: ParsecT s u m Char

    -- | This lexeme parser parses a literal string. Returns the literal
    -- string value. This parsers deals correctly with escape sequences and
    -- gaps. The literal string is parsed according to the grammar rules
    -- defined in the Haskell report (which matches most programming
    -- languages quite closely).

    , stringLiteral :: ParsecT s u m String

    -- | This lexeme parser parses a natural number (a positive whole
    -- number). Returns the value of the number. The number can be specified
    -- in 'decimal', 'hexadecimal' or 'octal'. The number is parsed
    -- according to the grammar rules in the Haskell report.

    , natural :: ParsecT s u m Integer

    -- | This lexeme parser parses an integer (a whole number). This parser
    -- is like 'natural' except that it can be prefixed with sign
    -- (i.e. \'-\' or \'+\'). Returns the value of the number. The number
    -- can be specified in 'decimal', 'hexadecimal' or 'octal'. The number
    -- is parsed according to the grammar rules in the Haskell report.

    , integer :: ParsecT s u m Integer

    -- | This lexeme parser parses a floating point value. Returns the value
    -- of the number. The number is parsed according to the grammar rules
    -- defined in the Haskell report.

    , float :: ParsecT s u m Double

    -- | This lexeme parser parses either 'natural' or a 'float'.
    -- Returns the value of the number. This parsers deals with any overlap
    -- in the grammar rules for naturals and floats. The number is parsed
    -- according to the grammar rules defined in the Haskell report.

    , naturalOrFloat :: ParsecT s u m (Either Integer Double)

    -- | Parses a positive whole number in the decimal system. Returns the
    -- value of the number.

    , decimal :: ParsecT s u m Integer

    -- | Parses a positive whole number in the hexadecimal system. The number
    -- should be prefixed with \"0x\" or \"0X\". Returns the value of the
    -- number.

    , hexadecimal :: ParsecT s u m Integer

    -- | Parses a positive whole number in the octal system. The number
    -- should be prefixed with \"0o\" or \"0O\". Returns the value of the
    -- number.

    , octal :: ParsecT s u m Integer

    -- | Lexeme parser @symbol s@ parses 'string' @s@ and skips
    -- trailing white space.

    , symbol :: String -> ParsecT s u m String

    -- | @lexeme p@ first applies parser @p@ and than the 'whiteSpace'
    -- parser, returning the value of @p@. Every lexical token (lexeme) is
    -- defined using @lexeme@, this way every parse starts at a point
    -- without white space. Parsers that use @lexeme@ are called /lexeme/
    -- parsers in this document.
    --
    -- The only point where the 'whiteSpace' parser should be called
    -- explicitly is the start of the main parser in order to skip any
    -- leading white space.

    , lexeme :: forall a. ParsecT s u m a -> ParsecT s u m a

    -- | Parses any white space. White space consists of /zero/ or more
    -- occurrences of a 'space', a line comment or a block (multi line)
    -- comment. Block comments may be nested. How comments are started and
    -- ended is defined in the 'LanguageDef' that is passed to
    -- 'makeTokenParser'.

    , whiteSpace :: ParsecT s u m ()

    -- | Lexeme parser @parens p@ parses @p@ enclosed in parenthesis,
    -- returning the value of @p@.

    , parens :: forall a. ParsecT s u m a -> ParsecT s u m a

    -- | Lexeme parser @braces p@ parses @p@ enclosed in braces (\'{\' and
    -- \'}\'), returning the value of @p@.

    , braces :: forall a. ParsecT s u m a -> ParsecT s u m a

    -- | Lexeme parser @angles p@ parses @p@ enclosed in angle brackets (\'\<\'
    -- and \'>\'), returning the value of @p@.

    , angles :: forall a. ParsecT s u m a -> ParsecT s u m a

    -- | Lexeme parser @brackets p@ parses @p@ enclosed in brackets (\'[\'
    -- and \']\'), returning the value of @p@.

    , brackets :: forall a. ParsecT s u m a -> ParsecT s u m a

    -- | Lexeme parser |semi| parses the character \';\' and skips any
    -- trailing white space. Returns the string \";\".

    , semi :: ParsecT s u m String

    -- | Lexeme parser @comma@ parses the character \',\' and skips any
    -- trailing white space. Returns the string \",\".

    , comma :: ParsecT s u m String

    -- | Lexeme parser @colon@ parses the character \':\' and skips any
    -- trailing white space. Returns the string \":\".

    , colon :: ParsecT s u m String

    -- | Lexeme parser @dot@ parses the character \'.\' and skips any
    -- trailing white space. Returns the string \".\".

    , dot :: ParsecT s u m String

    -- | Lexeme parser @semiSep p@ parses /zero/ or more occurrences of @p@
    -- separated by 'semi'. Returns a list of values returned by @p@.

    , semiSep :: forall a . ParsecT s u m a -> ParsecT s u m [a]

    -- | Lexeme parser @semiSep1 p@ parses /one/ or more occurrences of @p@
    -- separated by 'semi'. Returns a list of values returned by @p@.

    , semiSep1 :: forall a . ParsecT s u m a -> ParsecT s u m [a]

    -- | Lexeme parser @commaSep p@ parses /zero/ or more occurrences of
    -- @p@ separated by 'comma'. Returns a list of values returned by @p@.

    , commaSep :: forall a . ParsecT s u m a -> ParsecT s u m [a]

    -- | Lexeme parser @commaSep1 p@ parses /one/ or more occurrences of
    -- @p@ separated by 'comma'. Returns a list of values returned by @p@.

    , commaSep1 :: forall a . ParsecT s u m a -> ParsecT s u m [a] }

-- Given a LanguageDef, create a token parser

-- | The expression @makeTokenParser language@ creates a 'TokenParser'
-- record that contains lexical parsers that are defined using the
-- definitions in the @language@ record.
--
-- The use of this function is quite stylized — one imports the appropriate
-- language definition and selects the lexical parsers that are needed from
-- the resulting 'TokenParser'.
--
-- > module Main (main) where
-- >
-- > import Text.Parsec
-- > import qualified Text.Parsec.Token as Token
-- > import Text.Parsec.Language (haskellDef)
-- >
-- > -- The parser
-- > ...
-- >
-- > expr =  parens expr
-- >     <|> identifier
-- >     <|> ...
-- >
-- > -- The lexer
-- > lexer      = Token.makeTokenParser haskellDef
-- >
-- > parens     = Token.parens     lexer
-- > braces     = Token.braces     lexer
-- > identifier = Token.identifier lexer
-- > reserved   = Token.reserved   lexer
-- > ...

makeTokenParser :: Stream s m Char => LanguageDef s u m -> TokenParser s u m
makeTokenParser languageDef =
    TokenParser
    { identifier     = identifier
    , reserved       = reserved
    , operator       = operator
    , reservedOp     = reservedOp

    , charLiteral    = charLiteral
    , stringLiteral  = stringLiteral
    , natural        = natural
    , integer        = integer
    , float          = float
    , naturalOrFloat = naturalOrFloat
    , decimal        = decimal
    , hexadecimal    = hexadecimal
    , octal          = octal

    , symbol         = symbol
    , lexeme         = lexeme
    , whiteSpace     = whiteSpace

    , parens         = parens
    , braces         = braces
    , angles         = angles
    , brackets       = brackets
    , semi           = semi
    , comma          = comma
    , colon          = colon
    , dot            = dot
    , semiSep        = semiSep
    , semiSep1       = semiSep1
    , commaSep       = commaSep
    , commaSep1      = commaSep1 }
    where

    -- bracketing

    parens    = between (symbol "(") (symbol ")")
    braces    = between (symbol "{") (symbol "}")
    angles    = between (symbol "<") (symbol ">")
    brackets  = between (symbol "[") (symbol "]")

    semi      = symbol ";"
    comma     = symbol ","
    dot       = symbol "."
    colon     = symbol ":"

    commaSep  = (`sepBy` comma)
    semiSep   = (`sepBy` semi)

    commaSep1 = (`sepBy1` comma)
    semiSep1  = (`sepBy1` semi)

    -- chars & strings

    charLiteral = lexeme ( between (char '\'')
                                   (char '\'' <?> "end of character")
                                   characterChar )
                  <?> "character"

    characterChar = charLetter <|> charEscape <?> "literal character"

    charEscape = char '\\' >> escapeCode
    charLetter = satisfy (\c -> (c /= '\'') && (c /= '\\') && (c > '\026'))

    stringLiteral =
        lexeme ((foldr (maybe id (:)) "" <$>
                 between (char '"') (char '"' <?> "end of string")
                             (many stringChar)) <?> "literal string")

    stringChar = (Just <$> stringLetter) <|> stringEscape <?> "string character"

    stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape = char '\\' >>
                   ( (escapeGap >> return Nothing)   <|>
                     (escapeEmpty >> return Nothing) <|>
                     (Just <$> escapeCode) )

    escapeEmpty = char '&'
    escapeGap   = many1 space >> char '\\' <?> "end of string gap"

    -- escape codes

    escapeCode = charEsc <|> charNum <|> charAscii <|> charControl
                 <?> "escape code"

    charControl = toEnum . subtract 64 . fromEnum <$> (char '^' >> upper)

    charNum = toEnum . fromInteger <$>
              ( decimal <|>
               (char 'o' >> number 8 octDigit) <|>
               (char 'x' >> number 16 hexDigit) )

    charEsc = choice (parseEsc <$> escMap)
        where parseEsc (c, code) = char c >> return code

    charAscii = choice (parseAscii <$> asciiMap)
        where parseAscii (asc, code) = try (string asc >> return code)

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

    -- numbers

    naturalOrFloat = lexeme natFloat <?> "number"

    float   = lexeme floating <?> "float"
    integer = lexeme int      <?> "integer"
    natural = lexeme nat      <?> "natural"

    -- floats

    floating = decimal >>= fractExponent

    natFloat = (char '0' >> zeroNumFloat) <|> decimalFloat

    zeroNumFloat = (Left <$> (hexadecimal <|> octal)) <|>
                   decimalFloat <|>
                   fractFloat 0 <|>
                   return (Left 0)

    decimalFloat = decimal >>= \n -> option (Left n) (fractFloat n)

    fractFloat n = Right <$> fractExponent n

    fractExponent n =
        do { fract <- fraction
           ; expo  <- option 1.0 exponent'
           ; return $ (n' + fract) * expo
           } <|> ((* n') <$> exponent')
        where n' = fromInteger n

    fraction =
        do { void $ char '.'
           ; digits <- many1 digit <?> "fraction"
           ; return $ foldr op 0.0 digits
           } <?> "fraction"
        where op d f = (f + fromIntegral (digitToInt d)) / 10.0

    exponent' =
        do { void $ oneOf "eE"
           ; f <- sign
           ; e <- decimal <?> "exponent"
           ; return $ power (f e)
           } <?> "exponent"
        where power e
                  | e < 0     = 1.0 / power (-e)
                  | otherwise = fromInteger (10^e)

    -- integers and naturals

    int = ($) <$> lexeme sign <*> nat

    sign = (char '-' >> return negate) <|> (char '+' >> return id) <|> return id

    nat = zeroNumber <|> decimal

    zeroNumber = (char '0' >> (hexadecimal <|> octal <|> decimal <|> return 0))
                 <?> ""

    decimal     = number 10 digit
    hexadecimal = oneOf "xX" >> number 16 hexDigit
    octal       = oneOf "oO" >> number 8  octDigit

    number base baseDigit = do
      digits <- many1 baseDigit
      let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
      n `seq` return n

    -- operators & reserved ops

    reservedOp name =
        lexeme $ try $ do
          void $ string name
          notFollowedBy (opLetter languageDef) <?> ("end of " ++ show name)

    operator =
        lexeme $ try $ do
          name <- oper
          if isReservedOp name
          then unexpected ("reserved operator " ++ show name)
          else return name

    oper = ((:) <$> opStart languageDef <*> many (opLetter languageDef))
           <?> "operator"

    isReservedOp = isReserved . sort $ reservedOpNames languageDef

    -- identifiers & reserved words

    reserved name =
        lexeme $ try $ do
          void $ caseString name
          notFollowedBy (identLetter languageDef) <?> ("end of " ++ show name)

    caseString name
        | caseSensitive languageDef = string name
        | otherwise                 = walk name >> return name
        where walk = foldr (\c -> ((caseChar c <?> show name) >>)) (return ())
              caseChar c
                  | isAlpha c = char (toLower c) <|> char (toUpper c)
                  | otherwise = char c

    identifier =
        lexeme $ try $ do
          name <- ident
          if isReservedName name
          then unexpected ("reserved word " ++ show name)
          else return name

    ident = ((:) <$> identStart languageDef <*> many (identLetter languageDef))
            <?> "identifier"

    isReservedName name = isReserved theReservedNames caseName
        where caseName
                  | caseSensitive languageDef = name
                  | otherwise                 = toLower <$> name

    isReserved names name = scan names
        where scan []     = False
              scan (r:rs) = case compare r name of
                              LT  -> scan rs
                              EQ  -> True
                              GT  -> False

    theReservedNames
        | caseSensitive languageDef = sort reserved
        | otherwise                 = sort . fmap (fmap toLower) $ reserved
        where reserved = reservedNames languageDef

    -- white space & symbols

    symbol = lexeme . string

    lexeme p = p <* whiteSpace

    whiteSpace
        | noLine && noMulti = skipMany (simpleSpace      <?> "")
        | noLine            = skipMany (simpleSpace      <|>
                                        multiLineComment <?> "")
        | noMulti           = skipMany (simpleSpace      <|>
                                        oneLineComment   <?> "")
        | otherwise         = skipMany (simpleSpace      <|>
                                        oneLineComment   <|>
                                        multiLineComment <?> "")
        where
          noLine  = null (commentLine languageDef)
          noMulti = null (commentStart languageDef)

    simpleSpace = skipMany1 (satisfy isSpace)

    oneLineComment = void (try (string (commentLine languageDef))
                          >> skipMany (satisfy (/= '\n')))

    multiLineComment = try (string (commentStart languageDef)) >> inComment

    inComment = if nestedComments languageDef
                then inCommentMulti
                else inCommentSingle

    inCommentMulti
        =  void (try . string $ commentEnd languageDef)
       <|> (multiLineComment            >> inCommentMulti)
       <|> (skipMany1 (noneOf startEnd) >> inCommentMulti)
       <|> (oneOf startEnd              >> inCommentMulti)
       <?> "end of comment"

    inCommentSingle
        =  void (try . string $ commentEnd languageDef)
       <|> (skipMany1 (noneOf startEnd) >> inCommentSingle)
       <|> (oneOf startEnd              >> inCommentSingle)
       <?> "end of comment"

    startEnd = nub $ (++) <$> commentEnd <*> commentStart $ languageDef
