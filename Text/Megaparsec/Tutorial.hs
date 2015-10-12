-- |
-- Module      :  Text.Megaparsec.Tutorial
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A tutorial for Megaparsec (and an introduction to monadic parsing
-- combinators in general).

module Text.Megaparsec.Tutorial
(
  -- * Introduction
  -- $introduction

  -- * Dissecting a basic parser
  -- $dissecting

  -- * Alternatives to do notation
  -- $do-alternatives

  -- * Repetition
  -- $repetition

  -- * Choice
  -- $choice
)
where

import Text.Megaparsec
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String (Parser)
import Control.Applicative
import Control.Monad

{- $introduction

/Parsing/ means taking text (or, say, a stream of bytes) and turning it into data; a parser is simply a function that can do that. For instance, 'read' parses a number from a string:

>>> read "123" :: Int
123

Not all strings represent numbers, of course. If you try to 'read' an invalid string, you would get an error:

>>> read "123xyz" :: Int
*** Exception: Prelude.read: no parse

Megaparsec is a library that helps you write parsers that are fast and produce good error messages when something goes wrong. For instance, if 'read' was a bit smarter, it could give you the following error message instead:

>>> read "123xyz"
*** Exception: line 1, column 4: expected a digit, but found 'x'

Good error messages are invaluable when you are parsing something more complicated than a number. (Some examples might be: an arithmetic expression, some JSON-encoded data, or even a programming language!)

To get a taste of Megaparsec, let's first write a simple parser in plain Haskell and then rewrite it using Megaparsec. Our data looks like this: @{123,456}@ (so, just 2 numbers in curly braces, separated by a comma).

A parser that doesn't handle errors is pretty easy – just split the string at the comma, strip braces, and read the numbers:

@
pair :: String -> (Integer, Integer)
pair s =
  let (a, b) = break (== ',') s
  in  (read (tail a), read (tail (init b)))
@

Unfortunately, it also doesn't even check whether the numbers are actually in /braces/ – it just removes the first and last characters. As parsers go, this one is pretty lousy.

Here's the same parser with added error reporting – better, but also quite complicated:

@
pair :: String -> (Integer, Integer)
pair s = (an, bn)
  where
    -- First try to split the string in 2 pieces.
    (a, b) = if ',' \`elem\` s
               then break (== ',') s
               else error "no comma"
    -- Remove the brace from the 1st piece.
    a' = if null a || head a /= '{'
           then error "no opening brace"
           else tail a
    -- Remove the comma and brace from the 2nd piece.
    b' = if null b || last b /= '}'
           then error "no closing brace"
           else tail (init b)
    -- Finally, parse pieces as numbers.
    an = case 'Text.Read.readMaybe' a' of
           Nothing -> error "can't parse 1st piece as a number"
           Just x  -> x
    bn = case 'Text.Read.readMaybe' b' of
           Nothing -> error "can't parse 2nd piece as a number"
           Just x  -> x
@

Finally, here's the same parser written with Megaparsec:

@
pairParser :: 'Parser' (Integer, Integer)
pairParser = do
  'char' '{'     '<?>' "opening brace"
  a \<- 'integer' '<?>' "first number"
  'char' ','     '<?>' "comma"
  b \<- 'integer' '<?>' "second number"
  'char' '}'     '<?>' "closing brace"
  return (a, b)

pair :: String -> (Integer, Integer)
pair s = case 'parse' (pairParser '<*' 'eof') "" s of
  Left errorMessage -> error (show errorMessage)
  Right result -> result
@

Not only it produces error messages automatically, but it even tells you the exact place where errors occur (which is really useful when you're parsing something long):

>>> pair "12,34}"
*** Exception: line 1, column 1:
unexpected '1'
expecting opening brace

>>> pair "{1a,23}"
*** Exception: line 1, column 3:
unexpected 'a'
expecting comma or rest of first number

Now, you may have noticed that @pairParser@ and @pair@ are separated; @pairParser@ is the actual parser, and @pair@ is a wrapper of sorts that calls @pairParser@, decides what to do when there's been an error during parsing, etc. The reason for that is composability. For instance, let's take 2 parsers: 'upperChar' and 'digitChar' (available in Megaparsec out of the box). They roughly correspond to these functions:

@
-- Returns parsed thing and the rest of the string.
upperChar :: String -> (Char, String)
upperChar [] = error "unexpected end of input, expecting uppercase letter"
upperChar (c:cs)
  | isUpper c = (c, cs)
  | otherwise = error ("unexpected " ++ show c ++ ", expecting uppercase letter")

digitChar :: String -> (Char, String)
digitChar [] = error "unexpected end of input, expecting digit"
digitChar (c:cs)
  | isDigit c = (c, cs)
  | otherwise = error ("unexpected " ++ show c ++ ", expecting digit")
@

Unfortunately, written like this they are non-reusable (in other words, we can't combine these 2 functions into a function that would parse either a digit or an uppercase letter). On the other hand, we can combine 'Parser's pretty easily using '<|>':

>>> parseTest (digitChar <|> upperChar) "a"
parse error at line 1, column 1:
unexpected 'a'
expecting digit or uppercase letter
>>> parseTest (digitChar <|> upperChar) "A"
'A'
>>> parseTest (digitChar <|> upperChar) "1"
'1'

So, what does Megaparsec consist of?

* predefined parsers ('upperChar', 'integer', etc)
* functions to combine those parsers together in various ways
* functions to run parsers by applying them to strings (and not only strings)

In the following sections we'll learn how to use all of those.
-}

{- $dissecting

Let's take a look at @pairParser@ again (with added comments):

@
pairParser :: 'Parser' (Integer, Integer)
pairParser = do
  -- expect '{' in the input
  'char' '{' '<?>' "opening brace"

  -- expect some digits to follow; if found, parse them as a number
  -- and assign the result to “a”
  a \<- 'integer' '<?>' "first number"

  -- expect ',' to follow
  'char' ',' '<?>' "comma"

  -- expect another number to follow, and assign the parsed number to “b”
  b \<- 'integer' '<?>' "second number"

  -- expect '}' to follow
  'char' '}' '<?>' "closing brace"

  -- the final value produced by the parser is a pair of parsed numbers
  return (a, b)
@

Now, you might have some questions:

  * __What does “expect character X to follow” mean?__

    The parser will look at the next character in the input, discard (or “eat”, or “consume”) it if it's X, and throw an error if it's not X.

  * __What does '<?>' do?__

    It gives a name to a parser, which will be used if the parser throws an error. For instance, when 'integer' doesn't see any digits, by default the error will look like “expecting integer”, but with '<?>' it will change to “expecting first number”.

    You don't have to give names to /all/ your parsers (as I did in this example) – the default ones are often good enough.

  * __What will happen if there are more characters after the closing brace?__

    Nothing. In general parsers should consume some part of the input, return a result, and not even care about the rest of the input, because otherwise it becomes hard to combine parsers together (just imagine 'integer' requiring the input to end after the last digit, for example). However, since ultimately we /don't/ want any input after the closing brace, we require it in @pair@ by writing @'parse' (pairParser '<*' 'eof')@ instead of @'parse' pairParser@.

  * __What does 'parse' do?__

    It gives the parser a string as an input, runs the parser, and returns whatever the parser returns (so, either an error message or the produced value). There are other functions for running parsers – for instance, 'parseTest' doesn't return anything but just prints the result (and thus it can be useful for testing your parsers).
-}

{- $do-alternatives

You already know how to assemble bigger parsers from smaller ones – just use do notation. However, sometimes other ways lead to clearer and shorter code, so it's nice to know them too. As a motivating example, here's @pairParser@ rewritten in 2 lines of code (which may seem confusing at this moment, but bear with me):

@
braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

pairParser :: Parser (Integer, Integer)
pairParser = braces ((,) \<$\> (integer \<* char ',') \<*\> integer)
@

And now, an explanation of how this all works.

First of all, you can use '*>' and '<*' to chain parsers. @p *\> q@ means “execute @p@ and then @q@”; @p \<* q@ means “execute @p@, then @q@, but return @p@'s result”. For instance, here's a function called 'between' that takes a parser and surrounds it from left and right by other parsers:

>>> parseTest (between (char '<') (char '>') integer) "<123>"
123

And here are 2 ways to write it:

  * with do notation:

    @
    between :: Parser x -> Parser y -> Parser a -> Parser a
    between open close p = do
      open
      result \<- p
      close
      return result
    @

  * with '*>' and '<*':

    @
    between :: Parser x -> Parser y -> Parser a -> Parser a
    between open close p = open *\> p \<* close
    @

    ('*>' and '<*' have equal precedence, so it will be interpreted as @(open *\> p) \<* close@.)

Then, you can also use '<$>' and '<*>' or one of @liftA*@ functions to apply functions to parsers' results without having to assign those results to variables:

  * with do notation:

    @
    a \<- someParser
    b \<- otherParser
    c \<- thirdParser
    return (a, b, c)
    @

  * with '<$>' and '<*>' (if the function has more arguments, just keep applying '<*>'):

    @
    (,,) \<$\> someParser \<*\> otherParser \<*\> thirdParser
    @

  * with 'liftA3' (there are also 'liftA' and 'liftA2' available):

    @
    liftA3 (,,) someParser otherParser thirdParser
    @

Of course, it works with any function, not just @(,)@ or @(,,)@.

Using both of these techniques, we can finally rewrite @pairParser@ in /applicative style/. Here's our motivating example again:

@
braces :: Parser a -> Parser a
braces = between (char '{') (char '}')

pairParser :: Parser (Integer, Integer)
pairParser = braces ((,) \<$\> (integer \<* char ',') \<*\> integer)
@

Look at the original version and this one and try to understand how they are equivalent – since parsers are quite often written like this, you should be familiar with this style in order to be able to read others' code. (Note that while parsers written in applicative style are usually much shorter than the same parsers written with do notation, they are also often harder to read and understand. A somewhat common notion among beginners is that /all/ parsers should be written in applicative style; try not to fall into this trap. Prefer readability over terseness.)
-}

{- $repetition

The most basic repetition combinator is 'many':

@
many :: Parser a -> Parser [a]
@

It takes a parser and applies it as long as it can, then returns the results in a list:

>>> parseTest (many digitChar) "123foo"
"123"
>>> parseTest (many (integer <* char '.')) "3.14.15."
[3,14,15]

Note that if the parser can't be applied even once, 'many' will return an empty list (in other words, zero repetitions is “many” repetitions too):

>>> parseTest (many digitChar) "foo"
""

A lot of parsers can be trivially implemented with 'many'. For instance, 'space' (which skips any amount of spaces, newlines, tabs, etc):

@
space :: Parser ()
space = do
  'many' 'spaceChar'
  return ()
@

@return ()@ is used here to avoid returning spaces as a string, since they most likely aren't needed anyway.

A slightly prettier way to write 'space' is to use 'void':

@
space = 'void' ('many' 'spaceChar')
@

An even better way is to use 'skipMany', which is a combination of 'void' and 'many' (but, unlike 'void', is self-explanatory):

@
space = 'skipMany' 'spaceChar'
@

You may wonder: if 'many' allows zero repetitions, wouldn't it cause problems for 'space'? For instance, if we're parsing 2 numbers separated by space – e.g. @"12 34"@ – we wouldn't want @"1234"@ to be accepted. How does 'space' deal with it?

The answer is that in many cases it doesn't matter. Even though 'space' accepts zero spaces, @"1234"@ won't ever be parsed as @"12 34"@ because @"34"@ will be treated as a part of the first number:

>>> let firstNum  = integer <?> "first number"
>>> let secondNum = integer <?> "second number"
>>> parseTest (liftA2 (,) firstNum (space *> secondNum)) "1234"
parse error at line 1, column 5:
unexpected end of input
expecting rest of first number, second number, or white space

@firstNum@ will be consuming digits as long as it can, so there simply won't be an opportunity for 'space' to work until a non-digit character occurs in the input – and if this non-digit character isn't a space, it won't parse anyway:

>>> parseTest (liftA2 (,) firstNum (space *> secondNum)) "12x34"
parse error at line 1, column 3:
unexpected 'x'
expecting rest of first number, second number, or white space

However, you could've noticed that the error messages are still somewhat misleading – they say “expecting second number”, but we already know that there /has/ to be something between the first and second number. Moreover, in some other cases 'space''s behavior is unacceptable (for instance, if we're parsing an arithmetic expression and we demand spaces between numbers and operators). Luckily, it's very easy to write a different 'space' with a combinator called 'some', which is exactly like 'many' but requires at least one repetition of the parser:

@
someSpace :: Parser ()
someSpace = 'void' ('some' 'spaceChar')    -- or “'skipSome' 'spaceChar'”
@

Now the error message looks better:

>>> parseTest (liftA2 (,) firstNum (someSpace *> secondNum)) "1234"
parse error at line 1, column 5:
unexpected end of input
expecting rest of first number or white space

By the way, 'integer' itself is implemented with 'some':

@
integer :: Parser Integer
integer = read \<$\> some digitChar \<?\> "integer"
@

Lastly, we can put further constraints on the number of repetitions with 'count' and 'count''. Use 'count' to repeat a parser a fixed number of times:

>>> parseTest (count 3 letterChar) "abcdef"
"abc"
>>> parseTest (count 3 letterChar) "ab"
parse error at line 1, column 3:
unexpected end of input
expecting letter

@'count'' n m@ repeats a parser at least @n@ times, but not more than @m@:

>>> parseTest (count' 3 5 letterChar) "ab"
parse error at line 1, column 3:
unexpected end of input
expecting letter
>>> parseTest (count' 3 5 letterChar) "abcd"
"abcd"
>>> parseTest (count' 3 5 letterChar) "abcdef"
"abcde"
-}

{- $choice

So far we've been only composing parsers sequentially – “expect this, then expect that”. However, we can also compose parsers in parallel – “expect either this or that”. Let's see how.

We've already seen the basic choice combinator ('<|>') previously in one of the examples:

>>> parseTest (digitChar <|> upperChar) "a"
parse error at line 1, column 1:
unexpected 'a'
expecting digit or uppercase letter

A more interesting example would be implementing 'many' and 'some' with '<|>'. What is 'many'? It's either no repetitions (in which case we can just return an empty list) or one-or-more repetitions:

@
many :: Parser a -> Parser [a]
many p = some p \<|\> return []
@

('<|>' tries alternatives left-to-right, so if you wrote @(return [] \<|\> some p)@, it would always return an empty list.)

Now, 'some' is simply a guaranteed single application of the parser, followed by more repetitions (possibly none at all):

@
some :: Parser a -> Parser [a]
some p = do
  x \<- p
  xs \<- many p
  return (x:xs)
@

Note that both arguments of `<|>` must have the same type. For instance, if you are parsing some numbers separated by commas or spaces, you might define your separator as @(char ',' \<|\> space)@. However, it won't work:

>>> parseTest (char ',' <|> space) ","
<interactive>:25-29:
    Couldn't match type ‘()’ with ‘Char’
    Expected type: ParsecT [Char] Identity Char
      Actual type: ParsecT [Char] Identity ()
    In the second argument of ‘(<|>)’, namely ‘space’
    In the first argument of ‘parseTest’, namely ‘(char ',' <|> space)’

When you don't care about the returned value (as in this case), you can just use 'void' to turn some of the return types into @()@:

>>> parseTest (void (char ',') <|> space) ","
()

When you /do/ care about the returned value, you'll have to decide what to return in each case. For example, let's say you're parsing a form field that contains a number, and sometimes the field will contain “-” instead of a number. What should happen?

One way is to represent “-” as 'Nothing' (and then you'd also have to wrap 'integer' into 'Just'):

>>> parseTest ((Just <$> integer) <|> (char '-' *> return Nothing)) "123"
Just 123
>>> parseTest ((Just <$> integer) <|> (char '-' *> return Nothing)) "-"
Nothing

Another way is to just interpret “-” as some predefined number, e.g. 0:

>>> parseTest (integer <|> (char '-' *> return 0)) "123"
123
>>> parseTest (integer <|> (char '-' *> return 0)) "-"
0

This could be written a bit differently with '<$':

>>> parseTest (integer <|> 0 <$ char '-') "123"
123
>>> parseTest (integer <|> 0 <$ char '-') "-"
0

Sometimes it's useful to treat /any/ bad input differently – i.e. not just “-” but anything unparseable. You already can do it easily with '<|>' and 'return', but there are 2 other combinators specifically for that purpose – 'optional' and 'option'.

'optional' takes a parser and returns 'Nothing' if the parser fails:

@
optional :: Parser a -> Parser (Maybe a)
optional p = (Just \<$\> p) \<|\> return Nothing
@

'option' takes a parser and a default value (which is, again, returned when the parser fails):

@
option :: a -> Parser a -> Parser a
option x p = p \<|\> return x
@
-}
