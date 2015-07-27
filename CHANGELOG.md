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
