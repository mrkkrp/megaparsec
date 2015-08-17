# Megaparsec

*Note that this is work in progress.*

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Build Status](https://travis-ci.org/mrkkrp/megaparsec.svg?branch=master)](https://travis-ci.org/mrkkrp/megaparsec)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/megaparsec/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/megaparsec?branch=master)

This is industrial-strength monadic parser combinator library. Megaparsec is
a fork of Parsec library originally written by Daan Leijen.

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
  Parsec (just look at how Parsec generate error-messages, you can find more
  in [issue #14](https://github.com/mrkkrp/megaparsec/issues/14) of
  Megaparsec, this is just one example).

* Megaparsec looks into the future, it does not contain code that serves for
  compatibility purposes, it also requires more recent version of `base`.

## Contributing

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the Github issue tracker for this project](https://github.com/mrkkrp/megaparsec/issues).

Pull-requests are also welcome (and yes, they will get attention and will be
merged quickly if they are good, we are progressive folks).

## License

Copyright © 2015 Megaparsec contributors<br>
Copyright © 2007 Paolo Martini<br>
Copyright © 1999–2000 Daan Leijen

Distributed under BSD3 license.
