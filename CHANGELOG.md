## Megaparsec 4.2.0

* Made `newPos` constructor and other functions in `Text.Megaparsec.Pos`
  smarter. Now it's impossible to create `SourcePos` with non-positive line
  number or column number. Unfortunately we cannot use `Numeric.Natural`
  because we need to support older versions of `base`.

* `ParseError` is now a monoid. `mergeError` is used as `mappend`.

* Added functions `addErrorMessages` and `newErrorMessages` to add several
  messages to existing error and to construct error with several attached
  messages respectively.

* `parseFromFile` now lives in `Text.Megaparsec.Prim`. Previously we had 5
  nearly identical definitions of the function, varying only in
  type-specific `readFile` function. Now the problem is solved by
  introduction of `StorableStream` type class. All supported stream types
  are instances of the class out of box and thus we have polymorphic version
  of `parseFromFile`.

* `ParseError` is now instance of `Exception` (and `Typeable`).

* Introduced `runParser'` and `runParserT'` functions that take and return
  parser state. This makes it possible to partially parse input, resume
  parsing, specify non-standard initial textual position, etc.

* Introduced `failure` function that allows to fail with arbitrary
  collection of messages. `unexpected` is now defined in terms of
  `failure`. One consequence of this design decision is that `failure` is
  now method of `MonadParsec`, while `unexpected` is not.

* Removed deprecated combinators from `Text.Megaparsec.Combinator`:

    * `chainl`
    * `chainl1`
    * `chainr`
    * `chainr1`

## Megaparsec 4.1.1

* Fixed bug in implementation of `sepEndBy` and `sepEndBy1` and removed
  deprecation notes for these functions.

* Added tests for `sepEndBy` and `sepEndBy1`.

## Megaparsec 4.1.0

* Relaxed dependency on `base`, so that minimal required version of `base`
  is now 4.6.0.0. This allows Megaparsec to compile with GHC 7.6.x.

* `Text.Megaparsec` and `Text.Megaparsec.Prim` do not export data types
  `Consumed` and `Reply` anymore because they are rather low-level
  implementation details that should not be visible to end-user.

* Representation of file name and textual position in error messages was
  made conventional.

* Fixed some typos is documentation and other materials.

## Megaparsec 4.0.0

### General changes

* Renamed `many1` → `some` as well as other parsers that had `many1` part in
  their names.

* The following functions are now re-exported from `Control.Applicative`:
  `(<|>)`, `many`, `some`, `optional`. See #9.

* Introduced type class `MonadParsec` in the style of MTL monad
  transformers. Eliminated built-in user state since it was not flexible
  enough and can be emulated via stack of monads. Now all tools in
  Megaparsec work with any instance of `MonadParsec`, not only with
  `ParsecT`.

* Added new function `parseMaybe` for lightweight parsing where error
  messages (and thus file name) are not important and entire input should be
  parsed. For example it can be used when parsing of single number according
  to specification of its format is desired.

* Fixed bug with `notFollowedBy` always succeeded with parsers that don't
  consume input, see #6.

* Flipped order of arguments in the primitive combinator `label`, see #21.

* Renamed `tokenPrim` → `token`, removed old `token`, because `tokenPrim` is
  more general and original `token` is little used.

* Made `token` parser more powerful, now its second argument can return
  `Either [Message] a` instead of `Maybe a`, so it can influence error
  message when parsing of token fails. See #29.

* Added new primitive combinator `hidden p` which hides “expected” tokens in
  error message when parser `p` fails.

* Tab width is not hard-coded anymore. It can be manipulated via
  `getTabWidth` and `setTabWidth`. Default tab-width is `defaultTabWidth`,
  which is 8.

### Error messages

* Introduced type class `ShowToken` and improved representation of
  characters and strings in error messages, see #12.

* Greatly improved quality of error messages. Fixed entire
  `Text.Megaparsec.Error` module, see #14 for more information. Made
  possible normal analysis of error messages without “render and re-parse”
  approach that previous maintainers had to practice to write even simplest
  tests, see module `Utils.hs` in `old-tests` for example.

* Reduced number of `Message` constructors (now there are only `Unexpected`,
  `Expected`, and `Message`). Empty “magic” message strings are ignored now,
  all the library now uses explicit error messages.

* Introduced hint system that greatly improves quality of error messages and
  made code of `Text.Megaparsec.Prim` a lot clearer.

### Built-in combinators

* All built-in combinators in `Text.Megaparsec.Combinator` now work with any
  instance of `Alternative` (some of them even with `Applicaitve`).

* Added more powerful `count'` parser. This parser can be told to parse from
  `m` to `n` occurrences of some thing. `count` is defined in terms of
  `count'`.

* Removed `optionMaybe` parser, because `optional` from
  `Control.Applicative` does the same thing.

* Added combinator `someTill`.

* These combinators are considered deprecated and will be removed in future:

    * `chainl`
    * `chainl1`
    * `chainr`
    * `chainr1`
    * `sepEndBy`
    * `sepEndBy1`

### Character parsing

* Renamed some parsers:

    * `alphaNum` → `alphaNumChar`
    * `digit` → `digitChar`
    * `endOfLine` → `eol`
    * `hexDigit` → `hexDigitChar`
    * `letter` → `letterChar`
    * `lower` → `lowerChar`
    * `octDigit` → `octDigitChar`
    * `space` → `spaceChar`
    * `spaces` → `space`
    * `upper` → `upperChar`

* Added new character parsers in `Text.Megaparsec.Char`:

    * `asciiChar`
    * `charCategory`
    * `controlChar`
    * `latin1Char`
    * `markChar`
    * `numberChar`
    * `printChar`
    * `punctuationChar`
    * `separatorChar`
    * `symbolChar`

* Descriptions of old parsers have been updated to accent some
  Unicode-specific moments. For example, old description of `letter` stated
  that it parses letters from “a” to “z” and from “A” to “Z”. This is wrong,
  since it used `Data.Char.isAlpha` predicate internally and thus parsed
  many more characters (letters of non-Latin languages, for example).

* Added combinators `char'`, `oneOf'`, `noneOf'`, and `string'` which are
  case-insensitive variants of `char`, `oneOf`, `noneOf`, and `string`
  respectively.

### Lexer

* Rewritten parsing of numbers, fixed #2 and #3 (in old Parsec project these
  are number 35 and 39 respectively), added per bug tests.

    * Since Haskell report doesn't say anything about sign, `integer` and
      `float` now parse numbers without sign.

    * Removed `natural` parser, it's equal to new `integer` now.

    * Renamed `naturalOrFloat` → `number` — this doesn't parse sign too.

    * Added new combinator `signed` to parse all sorts of signed numbers.

* Transformed `Text.Parsec.Token` into `Text.Megaparsec.Lexer`. Little of
  Parsec's code remains in the new lexer module. New module doesn't impose
  any assumptions on user and should be vastly more useful and
  general. Hairy stuff from original Parsec didn't get here, for example
  built-in Haskell functions are used to parse escape sequences and the like
  instead of trying to re-implement the whole thing.

### Other

* Renamed the following functions:

    * `permute` → `makePermParser`
    * `buildExpressionParser` → `makeExprParser`

* Added comprehensive QuickCheck test suite.

* Added benchmarks.

## Parsec 3.1.9

* Many and various updates to documentation and package description
  (including the homepage links).

* Add an `Eq` instance for `ParseError`.

* Fixed a regression from 3.1.6: `runP` is again exported from module
  `Text.Parsec`.

## Parsec 3.1.8

* Fix a regression from 3.1.6 related to exports from the main module.

## Parsec 3.1.7

* Fix a regression from 3.1.6 related to the reported position of error
  messages. See bug #9 for details.

* Reset the current error position on success of `lookAhead`.

## Parsec 3.1.6

* Export `Text` instances from `Text.Parsec`.

* Make `Text.Parsec` exports more visible.

* Re-arrange `Text.Parsec` exports.

* Add functions `crlf` and `endOfLine` to `Text.Parsec.Char` for handling
  input streams that do not have normalized line terminators.

* Fix off-by-one error in `Token.charControl`.

## Parsec 3.1.4 & 3.1.5

* Bump dependency on `text`.

## Parsec 3.1.3

* Fix a regression introduced in 3.1.2 related to positions reported by
  error messages.
