## Megaparsec 5.3.1

* Various updates to the docs.

* Allowed `QuickCheck-2.10`.

## Megaparsec 5.3.0

* Added the `match` combinator that allows to get collection of consumed
  tokens along with result of parsing.

* Added the `region` combinator which allows to process parse errors
  happening when its argument parser is run.

* Added the `getNextTokenPosition`, which returns position where the next
  token in the stream begins.

* Defined `Semigroup` and `Monoid` instances of `ParsecT`.

* Dropped support for GHC 7.6.

* Added an `ErrorComponent` instance for `()`.

## Megaparsec 5.2.0

* Added `MonadParsec` instance for `RWST`.

* Allowed `many` to run parsers that do not consume input. Previously this
  signalled an `error` which was ugly. Of course, in most cases giving
  `many` a parser that do not consume input will lead to non-termination
  bugs, but there are legal cases when this should be allowed. The test
  suite now contains an example of this. Non-termination issues is something
  inherited from the power Megaparsec gives (with more power comes more
  responsibility), so that `error` case in `many` really does not solve the
  problem, it was just a little ah-hoc guard we got from Parsec's past.

* The criterion benchmark was completely re-written and a new weigh
  benchmark to analyze memory consumption was added.

* Performance improvements: `count` (marginal improvement, simpler
  implementation), `count'` (considerable improvement), and `many`
  (marginal improvement, simpler implementation).

* Added `stateTokensProcessed` field to parser state and helper functions
  `getTokensProcessed` and `setTokensProcessed`. The field contains number
  of processed tokens so far. This allows, for example, create wrappers that
  return just parsed fragment of input stream alongside with result of
  parsing. (It was possible before, but very inefficient because it required
  traversing entire input stream twice.)

* `IndentNone` option of `indentBlock` now picks whitespace after it like
  its sisters `IndentMany` and `IndentSome` do, see #161.

* Fixed a couple of quite subtle bugs in `indentBlock` introduced by
  changing behaviour of `skipLineComment` in version 5.1.0. See #178 for
  more information.

## Megaparsec 5.1.2

* Stopped using property tests with `dbg` helper to avoid flood of debugging
  info when test suite is run.

* Fixed the build with `QuickCheck` versions older than 2.9.0.

## Megaparsec 5.1.1

* Exported the `observing` primitive from `Text.Megaparsec`.

## Megaparsec 5.1.0

* Defined `displayException` for `ParseError`, so exceptions are displayed
  in human-friendly form now. This works with GHC 7.10 and later.

* Line comments parsed by `skipLineComment` now may end at the end of input
  and do not necessarily require a newline to be parsed correctly. See #119.

* Exposed `parseErrorTextPretty` function in `Text.Megaparsec.Error` to
  allow to render `ParseError`s without stack of source positions.

* Eliminated the `old-tests` test suite — Parsec legacy. The cases that are
  not already *obviously* covered in the main test suite were included into
  it.

* Added `Arbitrary` instances for the following data types: `Pos`,
  `SourcePos`, `ErrorItem`, `Dec`, `ParseError` and `State`. This should
  make testing easier without the need to add orphan instances every time.
  The drawback is that we start to depend on `QuickCheck`, but that's a fair
  price.

* The test suite now uses the combination of Hspec and the
  `hpesc-megaparsec` package, which also improved the latter (that package
  is the recommended way to test Megaparsec parsers).

* The `try` combinator now truly backtracks parser state when its argument
  parser fails (either consuming input or not). Most users will never notice
  the difference though. See #142.

* Added the `dbg` function that should be helpful for debugging.

* Added `observing` primitive combinator that allows to “observe” parse
  errors without ending parsing (they are returned in `Left`, while normal
  results are wrapped in `Right`).

* Further documentation improvements.

## Megaparsec 5.0.1

* Derived `NFData` instances for `Pos`, `InvalidPosException`, `SourcePos`,
  `ErrorItem`, `Dec`, `ParseError`, and `State`.

* Derived `Data` instance for `ParseError`, `Data` and `Typeable` instances
  for `SourcePos` and `State`.

* Minor documentation improvements.

## Megaparsec 5.0.0

### General changes

* Removed `parseFromFile` and `StorableStream` type-class that was necessary
  for it. The reason for removal is that reading from file and then parsing
  its contents is trivial for every instance of `Stream` and this function
  provides no way to use newer methods for running a parser, such as
  `runParser'`. So, simply put, it adds little value and was included in 4.x
  versions for compatibility reasons.

* Moved position-advancing function from arguments of `token` and `tokens`
  functions to `Stream` type class (named `updatePos`). The new function
  allows to handle custom streams of tokens where every token contains
  information about its position in stream better (for example when stream
  of tokens is produced with happy/alex).

* Support for include files (stack of positions instead of flat position)
  added. The new functions `pushPosition` and `popPosition` can be used to
  move “vertically” in the stack of positions. `getPosition` and
  `setPosition` still work on top (“current file”) level, but user can get
  full stack via `getParserState` if necessary. Note that `ParseError` and
  pretty-printing for it also support the new feature.

* Added type function `Token` associated with `Stream` type class. The
  function returns type of token corresponding to specific token stream.

* Type `ParsecT` (and also type synonym `Parsec`) are now parametrized over
  type of custom component in parse errors.

* Parameters of `MonadParsec` type class are: `e` — type of custom component
  in parse errors, `s` — type of input stream, and `m` — type of underlying
  monad.

* Type of `failure` primitive combinator was changed, now it accepts three
  arguments: set of unexpected items, set of expected items, and set of
  custom data.

* Type of `token` primitive combinator was changed, now in case of failure a
  triple-tuple is returned with elements corresponding to arguments of
  `failure` primitive. The `token` primitive can also be optionally given an
  argument of token type to use in error messages (as expected item) in case
  of end of input.

* `unexpected` combinator now accepts argument of type `ErrorItem` instead
  of plain `String`.

* General performance improvements and improvements in speed of some
  combinators, `manyTill` in particular.

### Error messages

* The module `Text.Megaparsec.Pos` was completely rewritten. The new module
  uses `Pos` data type with smart constructors to ensure that things like
  line and column number can be only positive. `SourcePos` on the other hand
  does not require smart constructors anymore and its constructors are
  exported. `Show` and `Read` instances of `SourcePos` are derived and
  pretty-printing is done with help of `sourcePosPretty` function.

* The module `Text.Megaparsec.Error` was completely rewritten. A number of
  new types and type-classes are introduced: `ErrorItem`, `Dec`,
  `ErrorComponent`, and `ShowErrorComponent`. `ParseError` does not need
  smart constructors anymore and its constructor and field selectors are
  exported. It uses sets (from the `containers` package) instead of sorted
  lists to enumerate unexpected and expected items. The new definition is
  also parametrized over token type and custom data type which can be passed
  around as part of parse error. Default “custom data” component is `Dec`,
  which see. All in all, we have completely well-typed and extensible error
  messages now. `Show` and `Read` instances of `ParseError` are derived and
  pretty-printing is done with help of `parseErrorPretty`.

* The module `Text.Megaparsec.ShowToken` was eliminated and type class
  `ShowToken` was moved to `Text.Megaparsec.Error`. The only method of that
  class in now named `showTokens` and it works on streams of tokens, where
  single tokes are represented by `NonEmpty` list with single element.

### Built-in combinators

* Combinators `oneOf`, `oneOf'`, `noneOf`, and `noneOf'` now accept any
  instance of `Foldable`, not only `String`.

### Lexer

* Error messages about incorrect indentation levels were greatly improved.
  Now every such message contains information about desired ordering between
  “reference” indentation level and actual indentation level as well as
  values of these levels. The information is stored in `ParseError` in
  well-typed form and can be pretty-printed when necessary. As part of this
  improvement, type of `indentGuard` was changed.

* `incorrectIndent` combinator is introduced in `Text.Megaparsec.Lexer`
  module. It allows to fail with detailed information regarding incorrect
  indentation.

* Introduced `scientific` parser that can parse arbitrary big numbers
  without error or memory overflow. `float` still returns `Double`, but it's
  defined in terms of `scientific` now. Since `Scientific` type can reliably
  represent integer values as well as floating point values, `number` now
  returns `Scientific` instead of `Either Integer Double` (`Integer` or
  `Double` can be extracted from `Scientific` value anyway). This in turn
  makes `signed` parser more natural and general, because we do not need
  ad-hoc `Signed` type class anymore.

* Added `skipBlockCommentNested` function that should help parse possibly
  nested block comments.

* Added `lineFold` function that helps parse line folds.

## Megaparsec 4.4.0

* Now state returned on failure is the exact state of parser at the moment
  when it failed, which makes incremental parsing feature much better and
  opens possibilities for features like “on-the-fly” recovering from parse
  errors.

* The `count` combinator now works with `Applicative` instances (previously
  it worked only with instances of `Alternative`). It's now also faster.

* `tokens` and parsers built upon it (such as `string` and `string'`)
  backtrack automatically on failure now, that is, when they fail, they
  never consume any input. This is done to make their consumption model
  match how error messages are reported (which becomes an important thing as
  user gets more control with primitives like `withRecovery`). This means,
  in particular, that it's no longer necessary to use `try` with
  `tokens`-based parsers. This new feature *does not* affect performance in
  any way.

* New primitive parser `withRecovery` added. The parser allows to recover
  from parse errors “on-the-fly” and continue parsing. Once parsing is
  finished, several parse errors may be reported or ignored altogether.

* `eitherP` combinator added.

* Removed `Enum` instance of `Message` type. This was Parsec's legacy that
  we should eliminate now. `Message` does not constitute enumeration,
  `toEnum` was never properly defined for it. The idea to use `fromEnum` to
  determine type of `Message` is also ugly, for this purpose new functions
  `isUnexpected`, `isExpected`, and `isMessage` are defined in
  `Text.Megaparsec.Error`.

* Minor tweak in signature of `MonadParsec` type class. Collection of
  constraints changed from `Alternative m, Monad m, Stream s t` to
  `Alternative m, MonadPlus m, Stream s t`. This is done to make it easier
  to write more abstract code with older GHC where such primitives as
  `guard` are defined for instances of `MonadPlus`, not `Alternative`.

## Megaparsec 4.3.0

* Canonicalized `Applicative`/`Monad` instances. Thanks to Herbert Valerio
  Riedel.

* Custom messages in `ParseError` are printed each on its own line.

* Now accumulated hints are not used with `ParseError` records that have
  only custom messages in them (created with `Message` constructor, as
  opposed to `Unexpected` or `Expected`). This strips “expected” line from
  custom error messages where it's unlikely to be relevant anyway.

* Added higher-level combinators for indentation-sensitive grammars:
  `indentLevel`, `nonIndented`, and `indentBlock`.

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

* `number` parser in `Text.Megaparsec.Lexer` now can be used with `signed`
  combinator to parse either signed `Integer` or signed `Double`.

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
