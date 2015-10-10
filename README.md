# Megaparsec

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/megaparsec.svg?style=flat)](https://hackage.haskell.org/package/megaparsec)
[![Build Status](https://travis-ci.org/mrkkrp/megaparsec.svg?branch=master)](https://travis-ci.org/mrkkrp/megaparsec)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/megaparsec/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/megaparsec?branch=master)

* [Features](#features)
  * [Core features](#core-features)
  * [Character parsing](#character-parsing)
  * [Permutation parsing](#permutation-parsing)
  * [Expression parsing](#expression-parsing)
  * [Lexer](#lexer)
* [Documentation](#documentation)
* [Tutorials](#tutorials)
* [Comparison with other solutions](#comparison-with-other-solutions)
  * [Megaparsec and Attoparsec](#megaparsec-and-attoparsec)
  * [Megaparsec and Parsec](#megaparsec-and-parsec)
  * [Megaparsec and Parsers](#megaparsec-and-parsers)
* [Authors](#authors)
* [Contribution](#contribution)
* [License](#license)

This is industrial-strength monadic parser combinator library. Megaparsec is
a fork of [Parsec](https://github.com/aslatter/parsec) library originally
written by Daan Leijen.

## Features

This project provides flexible solutions to satisfy common parsing
needs. The section describes them shortly. If you're looking for
comprehensive documentation, see
[section about documentation](#documentation).

### Core features

The package is built around `MonadParsec` MTL-style monad transformer. All
tools and features work with any instance of `MonadParsec`. You can achieve
various effects combining monad transformers, i.e. building monad
stack. Since most common monad transformers like `WriterT`, `StateT`,
`ReaderT` and others are instances of `MonadParsec`, you can wrap `ParsecT`
*in* these monads, achieving, for example, backtracking state.

On the other hand `ParsecT` is instance of many type classes as well. Most
useful of them are `Monad`, `Applicative`, `Alternative`, and `MonadParsec`.

The module `Text.Megaparsec.Combinator` (its functions are included in
`Text.Megaparsec`)contains traditional, general combinators that work with
any instance of `Alternative` and some even with instances of `Applicative`.

Role of `Monad`, `Applicative`, and `Alternative` should be obvious, so
let's enumerate methods of `MonadParsec` type class. The class represents
core, basic functions of Megaparsec parsing. The rest of library is built
via combination of these primitives:

* `unexpected` allows to fail specifying what exactly is unexpected.

* `label` allows to add a “label” to any parser, so when it fails user will
  see the label in error message where “expected” items are enumerated.

* `hidden` hides any parser from error messages altogether, this is
  officially recommended way to hide things, prefer it to `label ""`
  approach.

* `try` enables backtracking in parsing.

* `lookAhead` allows to parse something without consuming any input.

* `notFollowedBy` succeeds when its argument fails.

* `eof` only succeeds at the end of input.

* `token` is used to parse single token.

* `tokens` makes it easy to parse several tokens in a row.

* `getParserState` returns full parser state.

* `updateParserState` applies given function on parser state.

This list of core function is longer than in some other libraries. Our goal
was easy and readable implementation of functionality provided by every such
primitive, not minimal number of them. You can read comprehensive
description of every primitive function in Megaparsec documentation.

Megaparsec can currently work with the following types of input stream:

* `String` = `[Char]`

* `ByteString` (strict and lazy)

* `Text` (strict and lazy)

### Character parsing

Megaparsec has decent support for Unicode-aware character parsing. Functions
for character parsing live in `Text.Megaparsec.Char` module (they all are
included in `Text.Megaparsec`). The functions can be divided into several
categories:

* *Simple parsers* — parsers that parse certain character or several
  characters of the same kind. This includes `newline`, `crlf`, `eol`,
  `tab`, and `space`.

* *Parsers corresponding to categories of characters* parse single char that
  belongs to certain category of characters, for example: `controlChar`,
  `spaceChar`, `upperChar`, `lowerChar`, `printChar`, `digitChar`, and
  others.

* *General parsers* that allow you to parse single character you specify or
  one of given characters, or any character except for given ones, or
  character satisfying given predicate. Case-insensitive versions of the
  parsers are available.

* *Parsers for sequences of characters* parse strings. These are more
  efficient and provide better error messages than other approaches most
  programmers can come up with. Case-sensitive `string` parser is available
  as well as case-insensitive `string'`.

### Permutation parsing

For those who are interested in parsing of permutation phrases, there is
`Text.Megaparsec.Perm`. You should import the module explicitly, it's not
included in `Text.Megaparsec` module.

### Expression parsing

Megaparsec has solution for parsing of expressions. Take a look at
`Text.Megaparsec.Expr`. You should import the module explicitly, it's not
included in `Text.Megaparsec` module.

Given table of operators that describes their fixity and precedence, you can
construct a parser that will parse any expression involving the
operators. See documentation for comprehensive description of how it works.

### Lexer

`Text.Megaparsec.Lexer` is module that should help you write your lexer. If
you have used `Parsec` in the past, this module “fixes” its particularly
inflexible `Text.Parsec.Token`.

`Text.Megaparsec.Lexer` is intended to be imported qualified, it's not
included in `Text.Megaparsec`. The module doesn't impose how you should
write your parser, but certain approaches may be more elegant than
others. Especially important theme is parsing of write space, comments and
indentation.

Design of the module allows you quickly solve simple tasks and doesn't get
in your way when you want to implemented something less standard.

## Documentation

Megaparsec is well-documented. All functions and data-types are thoroughly
described. We pay attention to avoid outdated info or unclear phrases in our
documentation. See [current version of Megaparsec documentation on
Hackage](https://hackage.haskell.org/package/megaparsec) for yourself.

## Tutorials

Megaparsec includes always-up-to-date tutorial in its source code, see
[`Text.Megaparsec.Tutorial`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Pos.html).
Apart from this, you can visit
[site of the project](https://mrkkrp.github.io/megaparsec/) which has
several tutorials that should help you start with your parsing tasks. The
site also has instructions and tips for Parsec users who decide to switch.

## Comparison with other solutions

There are quite a few libraries that can be used for parsing in Haskell,
let's compare Megaparsec with some of them.

### Megaparsec and Attoparsec

[Attoparsec](https://github.com/bos/attoparsec) is another prominent Haskell
library for parsing. Although the both libraries deal with parsing, it's
usually easy to decide which you will need in particular project:

* *Attoparsec* is much faster but not that feature-rich. It should be used
  when you want to process large amounts of data where performance matter
  more than quality of error messages.

* *Megaparsec* is good for parsing of source code or other human-readable
  texts. It has better error messages and it's implemented as monad
  transformer.

So, if you work with something human-readable where size of input data is
usually not huge, just go with Megaparsec, otherwise Attoparsec may be a
better choice.

### Megaparsec and Parsec

Since Megaparsec is a fork of Parsec, it's necessary to list main
differences between the two libraries:

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

If you want to see detailed change log, `CHANGELOG.md` may be helpful.

To be honest Parsec's development has seemingly stagnated. It has no test
suite (only three per-bug tests), and all its releases beginning from
version 3.1.2 (according or its change log) were about introducing and
fixing regressions. Parsec is old and somewhat famous in Haskell community,
so we understand there will be some kind of inertia, but we advise you use
Megaparsec from now on because it solves many problems of original Parsec
project. If you think you still have a reason to use original Parsec, open
an issue.

### Megaparsec and Parsers

There is [parsers](https://hackage.haskell.org/package/parsers) package,
which is great. You can use it with Megaparsec or Parsec, but consider the
following:

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

## Authors

You can find complete list of contributors in `AUTHORS.md` file in this
repository. Thanks to all the people who propose features and ideas,
although they are not in `AUTHORS.md`, without them Megaparsec would not be
that good.

## Contribution

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/megaparsec/issues).

Pull requests are also welcome (and yes, they will get attention and will be
merged quickly if they are good, we are progressive folks).

If you want to write a tutorial to be hosted on Megaparsec's site, open an
issue or pull request [here](https://github.com/mrkkrp/megaparsec-site).

## License

Copyright © 2015 Megaparsec contributors<br>
Copyright © 2007 Paolo Martini<br>
Copyright © 1999–2000 Daan Leijen

Distributed under BSD3 license.
