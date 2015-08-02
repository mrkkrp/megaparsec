# Megaparsec

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Build Status](https://travis-ci.org/mrkkrp/megaparsec.svg?branch=master)](https://travis-ci.org/mrkkrp/megaparsec)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/megaparsec/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/megaparsec?branch=master)

This is industrial-strength monadic parser combinator library. Megaparsec is
a fork of original Parsec library written by Daan Leijen. This library is
different from Parsec in the following ways:

* Original Parsec consists of quite ancient code-base and has certain
  stylistic problems that's anyone who tries to compile Parsec with `-Wall`
  option can notice. This has been refreshed in Megaparsec. The changes are
  mainly cosmetic but not limited to them.

* Some quirks and old «buggy features» (as well as plain bugs) are fixed.

* Original Parsec uses rather weak collection of tests: a test per bug,
  obviously to prevent regression. Our aim is to write complete test-suite
  with QuickCheck to cover 100% of Megaparsec code. You can understand need
  for this test-suite if you look at `CHANGELOG.md` file that includes
  Parsec-era changes. The word «regression» mentioned quite frequently.

* Megaparsec looks into future, it does not contain code that serves for
  compatibility purposes, it also requires more recent version of `base`.

* Finally, we have fixed numerous typos and other minor flaws.

The reason for creating separate version of the project was inactivity of
its current maintainer who reduced active contributions to something like
one commit in three months and indefinitely delayed merging of our
contributions without any explanation. We wanted to improve Parsec and we
had some ideas how this can be achieved, so we decided to create our own
version of Parsec.

## Contributing

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the Github issue tracker for this project](https://github.com/mrkkrp/megaparsec/issues).

Pull-requests are also welcome (and yes they will get attention and will be
merged quickly if they are good).

## License

Copyright © 2015 Megaparsec contributors<br>
Copyright © 2007 Paolo Martini<br>
Copyright © 1999–2000 Daan Leijen

Distributed under BSD3 license.
