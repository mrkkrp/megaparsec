# Megaparsec

[![License FreeBSD](https://img.shields.io/badge/license-FreeBSD-brightgreen.svg)](http://opensource.org/licenses/BSD-2-Clause)
[![Hackage](https://img.shields.io/hackage/v/megaparsec.svg?style=flat)](https://hackage.haskell.org/package/megaparsec)
[![Stackage Nightly](http://stackage.org/package/megaparsec/badge/nightly)](http://stackage.org/nightly/package/megaparsec)
[![Stackage LTS](http://stackage.org/package/megaparsec/badge/lts)](http://stackage.org/lts/package/megaparsec)
[![Build Status](https://travis-ci.org/mrkkrp/megaparsec.svg?branch=master)](https://travis-ci.org/mrkkrp/megaparsec)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/megaparsec/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/megaparsec?branch=master)

* [Features](#features)
    * [Core features](#core-features)
    * [Error messages](#error-messages)
    * [Alex and Happy support](#alex-and-happy-support)
    * [Character parsing](#character-parsing)
    * [Permutation parsing](#permutation-parsing)
    * [Expression parsing](#expression-parsing)
    * [Lexer](#lexer)
* [Documentation](#documentation)
* [Tutorials](#tutorials)
* [Performance](#performance)
* [Comparison with other solutions](#comparison-with-other-solutions)
    * [Megaparsec and Attoparsec](#megaparsec-and-attoparsec)
    * [Megaparsec and Parsec](#megaparsec-and-parsec)
    * [Megaparsec and Parsers](#megaparsec-and-parsers)
* [Related packages](#related-packages)
* [Authors](#authors)
* [Contribution](#contribution)
* [License](#license)

This is an industrial-strength monadic parser combinator library. Megaparsec
is a fork of [Parsec](https://github.com/aslatter/parsec) library originally
written by Daan Leijen.

## Features

This project provides flexible solutions to satisfy common parsing
needs. The section describes them shortly. If you're looking for
comprehensive documentation, see the
[section about documentation](#documentation).

### Core features

The package is built around `MonadParsec`, a MTL-style monad
transformer. All tools and features work with any instance of
`MonadParsec`. You can achieve various effects combining monad transformers,
i.e. building monad stack. Since most common monad transformers like
`WriterT`, `StateT`, `ReaderT` and others are instances of `MonadParsec`,
you can wrap `ParsecT` *in* these monads, achieving, for example,
backtracking state.

On the other hand `ParsecT` is instance of many type classes as well. The
most useful ones are `Monad`, `Applicative`, `Alternative`, and
`MonadParsec`.

The module
[`Text.Megaparsec.Combinator`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Combinator.html)
(its functions are included in `Text.Megaparsec`) contains traditional,
general combinators that work with any instance of `Alternative` and some
even with instances of `Applicative`.

Role of `Monad`, `Applicative`, and `Alternative` should be obvious, so
let's enumerate methods of `MonadParsec` type class. The class represents
core, basic functions of Megaparsec parsing. The rest of library is built
via combination of these primitives:

* `failure` allows to fail with arbitrary collection of messages.

* `label` allows to add a “label” to any parser, so when it fails the user will
  see the label in the error message where “expected” items are enumerated.

* `hidden` hides any parser from error messages altogether, this is
  officially recommended way to hide things, prefer it to the `label ""`
  approach.

* `try` enables backtracking in parsing.

* `lookAhead` allows to parse something without consuming input.

* `notFollowedBy` succeeds when its argument fails, it does not consume
  input.

* `withRecovery` allows to recover from parse errors “on-the-fly” and
  continue parsing. Once parsing is finished, several parse errors may be
  reported or ignored altogether.

* `eof` only succeeds at the end of input.

* `token` is used to parse single token.

* `tokens` makes it easy to parse several tokens in a row.

* `getParserState` returns full parser state.

* `updateParserState` applies given function on parser state.

This list of core functions is longer than in some other libraries. Our goal
was efficient and readable implementation of functionality provided by every
such primitive, not minimal number of them. You can read the comprehensive
description of every primitive function in
[Megaparsec documentation](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Prim.html).

Megaparsec can currently work with the following types of input stream:

* `String` = `[Char]`

* `ByteString` (strict and lazy)

* `Text` (strict and lazy)

### Error messages

Megaparsec 5 introduces well-typed error messages and ability to use custom
data types to adjust the library to your domain of interest. No need to keep
your info as shapeless bunch of strings anymore.

The default error component (`Dec`) has constructors corresponding to `fail`
function and indentation-related error messages. It is a decent option that
should work out-of-box for most parsing needs, while you are free to use
your own custom error component when necessary with little effort.

This new design allowed Megaparsec 5 to have much more helpful error
messages for indentation-sensitive parsing instead of plain “incorrect
indentation” phrase.

### Alex and Happy support

Megaparsec works well with streams of tokens produced by tools like
Alex/Happy. Megaparsec 5 adds `updatePos` method to `Stream` type class that
gives you full control over textual positions that are used to report token
positions in error messages. You can update current position on per
character basis or extract it from token — all cases are covered.

### Character parsing

Megaparsec has decent support for Unicode-aware character parsing. Functions
for character parsing live in [`Text.Megaparsec.Char`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html) (they all are
included in `Text.Megaparsec`). The functions can be divided into several
categories:

* *Simple parsers* — parsers that parse certain character or several
  characters of the same kind. This includes `newline`, `crlf`, `eol`,
  `tab`, and `space`.

* *Parsers corresponding to categories of characters* parse single character
  that belongs to certain category of characters, for example:
  `controlChar`, `spaceChar`, `upperChar`, `lowerChar`, `printChar`,
  `digitChar`, and others.

* *General parsers* that allow you to parse a single character you specify
  or one of given characters, or any character except for given ones, or
  character satisfying given predicate. Case-insensitive versions of the
  parsers are available.

* *Parsers for sequences of characters* parse strings. These are more
  efficient and provide better error messages than other approaches most
  programmers can come up with. Case-sensitive `string` parser is available
  as well as case-insensitive `string'`.

### Permutation parsing

For those who are interested in parsing of permutation phrases, there is
[`Text.Megaparsec.Perm`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Perm.html). You have to import the module explicitly, it's not
included in the `Text.Megaparsec` module.

### Expression parsing

Megaparsec has a solution for parsing of expressions. Take a look at
[`Text.Megaparsec.Expr`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Expr.html). You have to import the module explicitly, it's not
included in the `Text.Megaparsec`.

Given a table of operators that describes their fixity and precedence, you
can construct a parser that will parse any expression involving the
operators. See documentation for comprehensive description of how it works.

### Lexer

[`Text.Megaparsec.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Lexer.html)
is a module that should help you write your lexer. If you have used `Parsec`
in the past, this module “fixes” its particularly inflexible
`Text.Parsec.Token`.

`Text.Megaparsec.Lexer` is intended to be imported qualified, it's not
included in `Text.Megaparsec`. The module doesn't impose how you should
write your parser, but certain approaches may be more elegant than
others. An especially important theme is parsing of white space, comments,
and indentation.

The design of the module allows you quickly solve simple tasks and doesn't
get in your way when you want to implement something less standard.

Since Megaparsec 5, all tools for indentation-sensitive parsing are
available in `Text.Megaparsec.Lexer` module — no third party packages
required.

## Documentation

Megaparsec is well-documented. All functions and data-types are thoroughly
described. We pay attention to avoid outdated info or unclear phrases in our
documentation. See the [current version of Megaparsec documentation on
Hackage](https://hackage.haskell.org/package/megaparsec) for yourself.

## Tutorials

You can visit [site of the project](https://mrkkrp.github.io/megaparsec/)
which has [several tutorials](https://mrkkrp.github.io/megaparsec/tutorials.html) that
should help you to start with your parsing tasks. The site also has
instructions and tips for Parsec users who decide to switch.

## Performance

Despite being quite flexible, Megaparsec is also faster than Parsec. The
repository includes benchmarks that can be easily used to compare Megaparsec
and Parsec. In most cases Megaparsec is faster, sometimes dramatically
faster. If you happen to have some other benchmarks, I would appreciate if
you add Megaparsec to them and let me know how it performs.

## Comparison with other solutions

There are quite a few libraries that can be used for parsing in Haskell,
let's compare Megaparsec with some of them.

### Megaparsec and Attoparsec

[Attoparsec](https://github.com/bos/attoparsec) is another prominent Haskell
library for parsing. Although the both libraries deal with parsing, it's
usually easy to decide which you will need in particular project:

* *Attoparsec* is much faster but not that feature-rich. It should be used
  when you want to process large amounts of data where performance matters
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
  well-structured and doesn't contain things useless to end users.

* Megaparsec's code is clearer and doesn't contain “magic” found in original
  Parsec.

* Megaparsec has well-typed error messages and custom error messages.

* Megaparsec can recover from parse errors “on the fly” and continue
  parsing.

* Megaparsec is faster.

If you want to see a detailed change log, `CHANGELOG.md` may be helpful.

To be honest Parsec's development has seemingly stagnated. It has no test
suite (only three per-bug tests), and all its releases beginning from
version 3.1.2 (according or its change log) were about introducing and
fixing regressions. Parsec is old and somewhat famous in Haskell community,
so we understand there will be some kind of inertia, but we advise you use
Megaparsec from now on because it solves many problems of original Parsec
project. If you think you still have a reason to use original Parsec, open
an issue.

### Megaparsec and Parsers

There is [Parsers](https://hackage.haskell.org/package/parsers) package,
which is great. You can use it with Megaparsec or Parsec, but consider the
following:

* It depends on *both* Attoparsec and Parsec, which means you always grab
  useless code installing it. This is ridiculous, by the way, because this
  package is supposed to be useful for parser builders, so they can write
  basic core functionality and get the rest “for free”. But with these
  useful functions you get two more parsers as dependencies.

* It currently has a bug in definition of `lookAhead` for various monad
  transformers like `StateT`, etc. which is visible when you create
  backtracking state via monad stack, not via built-in features.

We intended to use Parsers library in Megaparsec at some point, but aside
from already mentioned flaws the library has different conventions for
naming of things, different set of “core” functions, etc., different
approach to lexer. So it didn't happen, Megaparsec has minimal dependencies,
it is feature-rich and self-contained.

## Related packages

The following packages are designed to be used with Megaparsec:

* [`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec)
  — utilities for testing Megaparsec parsers with with
  [Hspec](https://hackage.haskell.org/package/hspec).

* [`cassava-megaparsec`](https://hackage.haskell.org/package/cassava-megaparsec) —
  Megaparsec parser of CSV files that plays nicely
  with [Cassava](https://hackage.haskell.org/package/cassava).

* [`tagsoup-megaparsec`](https://hackage.haskell.org/package/tagsoup-megaparsec)
  — a library for easily using [TagSoup](https://hackage.haskell.org/package/tagsoup)
  as a token type in Megaparsec.

## Authors

The project was started and is currently maintained by Mark Karpov. You can
find complete list of contributors in `AUTHORS.md` file in official
repository of the project. Thanks to all the people who propose features and
ideas, although they are not in `AUTHORS.md`, without them Megaparsec would
not be that good.

## Contribution

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/megaparsec/issues).

Pull requests are also welcome (and yes, they will get attention and will be
merged quickly if they are good, we are progressive folks).

If you want to write a tutorial to be hosted on Megaparsec's site, open an
issue or pull request [here](https://github.com/mrkkrp/megaparsec-site).

## License

Copyright © 2015–2016 Megaparsec contributors<br>
Copyright © 2007 Paolo Martini<br>
Copyright © 1999–2000 Daan Leijen

Distributed under FreeBSD license.
