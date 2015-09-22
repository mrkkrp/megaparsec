# Megaparsec

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/megaparsec.svg?style=flat)](https://hackage.haskell.org/package/megaparsec)
[![Build Status](https://travis-ci.org/mrkkrp/megaparsec.svg?branch=master)](https://travis-ci.org/mrkkrp/megaparsec)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/megaparsec/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/megaparsec?branch=master)

This is industrial-strength monadic parser combinator library. Megaparsec is
a fork of [Parsec](https://github.com/aslatter/parsec) library originally
written by Daan Leijen.

Megaparsec is different from Parsec in the following ways:

* Better error messages. We test our error messages using dense QuickCheck
  tests. Good error messages are just as important for us as correct return
  values of our parsers. Megaparsec will be especially useful if you write
  compiler or interpreter for some language.

* Some quirks and “buggy features” (as well as plain bugs) of original
  Parsec are fixed. There is no undocumented surprising stuff in Megaparsec.

* Better support for Unicode parsing in `Text.Megaparsec.Char`.

* Megaparsec has more powerful combinators and can parse languages where
  indentation matters.

* Comprehensive QuickCheck test suite covering nearly 100% of our code.

* We have benchmarks to detect performance regressions.

* Better documentation, with 100% of functions covered, without typos and
  obsolete information, with working examples. Megaparsec's documentation is
  well-structured and doesn't contain things useless to end user.

* Megaparsec's code is clearer and doesn't contain “magic” found in original
  Parsec.

* Megaparsec looks into the future, it does not contain code that serves for
  compatibility purposes, it also requires more recent version of `base`.

## Megaparsec vs Parsec

There are good reasons to use Parsec:

* You need to work with legacy code or with older versions of GHC (< 7.10).

And that's it. In other cases you should prefer Megaparsec for your own
sake. If you think you have a reason to use Parsec other than listed here,
open an issue. We are glad to hear from you.

## Megaparsec vs Parsers

There is [parsers](https://hackage.haskell.org/package/parsers) package,
which is great. You can use it, but consider the following:

* It depends on *both* Attoparsec and Parsec, which means you always grab
  useless code installing it. This is ridiculous, by the way, because this
  package is supposed to be useful for parser builders, so they can write
  basic core functionality and get the rest “for free”. But with these
  useful functions you get two more parsers as dependencies.

* It currently has a bug in definition of `lookAhead` for various monad
  transformers like `StateT`, etc. which is visible when you create
  backtracking state via monad stack, not via built-in features. See #27.

We intended to use Parsers library in Megaparsec at some point, but aside
from already mentioned flaws the library has different conventions for
naming of things, different set of “core” functions, etc., different
approach to lexer. So it didn't happen, Megaparsec has minimal dependencies,
it is feature-rich and self-contained.

## Contributing

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/megaparsec/issues).

Pull requests are also welcome (and yes, they will get attention and will be
merged quickly if they are good, we are progressive folks).

## Spread the Word

Many people still don't know about Megaparsec, you can help the project by
writing about it in a blog, creating a tutorial or something like that. This
is highly appreciated.

## License

Copyright © 2015 Megaparsec contributors<br>
Copyright © 2007 Paolo Martini<br>
Copyright © 1999–2000 Daan Leijen

Distributed under BSD3 license.
