## MegaParsec 4.0.0

* Cosmetic changes in entire source code, numerous improvements and
  elimination of warnings.

* Rewritten parsing of numbers, fixed #2 and #3 (in old Parsec project these
  are number 35 and 39 respectively), added per bug tests.

    * Since Haskell report doesn't say anything about sign, I've made
      `integer` and `float` parse numbers without sign.

    * Removed `natural` parser, it's equal to new `integer` now.

    * Renamed `naturalOrFloat` → `number` — this doesn't parse sign too.

    * Added new combinator `signed` to parse all sorts of signed numbers.

    * For the sake of convenience I've added `integer'`, `float'`, and
     `number'` combinators that also can parse signed numbers out of box.

* Renamed `many1` → `some` as well as other parsers that had `many1` part in
  their names.

* The following functions are now re-exported from ‘Control.Applicative’:
  `(<|>)`, `many`, `some`, `optional`. See #9.

* Added comprehensive QuickCheck test suite.

* Added benchmarks.

* Fixed typos in source code and other files.

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
