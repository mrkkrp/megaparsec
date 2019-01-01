# Megaparsec

[![License FreeBSD](https://img.shields.io/badge/license-FreeBSD-brightgreen.svg)](http://opensource.org/licenses/BSD-2-Clause)
[![Hackage](https://img.shields.io/hackage/v/megaparsec.svg?style=flat)](https://hackage.haskell.org/package/megaparsec)
[![Stackage Nightly](http://stackage.org/package/megaparsec/badge/nightly)](http://stackage.org/nightly/package/megaparsec)
[![Stackage LTS](http://stackage.org/package/megaparsec/badge/lts)](http://stackage.org/lts/package/megaparsec)
[![Build Status](https://travis-ci.org/mrkkrp/megaparsec.svg?branch=master)](https://travis-ci.org/mrkkrp/megaparsec)

* [Features](#features)
    * [Core features](#core-features)
    * [Error messages](#error-messages)
    * [Alex support](#alex-support)
    * [Character and binary parsing](#character-and-binary-parsing)
    * [Lexer](#lexer)
* [Documentation](#documentation)
* [Tutorials](#tutorials)
* [Performance](#performance)
* [Comparison with other solutions](#comparison-with-other-solutions)
    * [Megaparsec vs Attoparsec](#megaparsec-vs-attoparsec)
    * [Megaparsec vs Parsec](#megaparsec-vs-parsec)
    * [Megaparsec vs Trifecta](#megaparsec-vs-trifecta)
    * [Megaparsec vs Earley](#megaparsec-vs-earley)
* [Related packages](#related-packages)
* [Prominent projects that use Megaparsec](#prominent-projects-that-use-megaparsec)
* [Links to announcements and blog posts](#links-to-announcements-and-blog-posts)
* [Authors](#authors)
* [Contribution](#contribution)
* [License](#license)

This is an industrial-strength monadic parser combinator library. Megaparsec
is a feature-rich package that strikes a nice balance between speed,
flexibility, and quality of parse errors.

## Features

The project provides flexible solutions to satisfy common parsing needs. The
section describes them shortly. If you're looking for comprehensive
documentation, see the [section about documentation](#documentation).

### Core features

The package is built around `MonadParsec`, an MTL-style monad transformer.
Most features work with all instances of `MonadParsec`. One can achieve
various effects combining monad transformers, i.e. building a monadic stack.
Since the common monad transformers like `WriterT`, `StateT`, `ReaderT` and
others are instances of the `MonadParsec` type class, one can also wrap
`ParsecT` *in* these monads, achieving, for example, backtracking state.

On the other hand `ParsecT` is an instance of many type classes as well. The
most useful ones are `Monad`, `Applicative`, `Alternative`, and
`MonadParsec`.

Megaparsec includes all functionality that is typically available in
Parsec-like libraries and also features some combinators that are quite
unique to it:

* `failure` allows us to report a parse error with unexpected and expected
  items.
* `fancyFailure` provides a way to report custom parse errors.
* `withRecovery` can be used to recover from parse errors “on-the-fly” and
  continue parsing. Once parsing is finished, several parse errors may be
  reported or ignored altogether.
* `observing` makes it possible to “observe” parse errors without ending
  parsing (they are returned in `Left`, while normal results are wrapped in
  `Right`).

In addition to that, Megaparsec features high-performance combinators
similar to those found in [Attoparsec][attoparsec]:

* `tokens` makes it easy to parse several tokens in a row (`string` and
  `string'` are built on top of this primitive). This is about 100 times
  faster than matching a string token by token. `tokens` returns “chunk” of
  original input, meaning that if you parse `Text`, it'll return `Text`
  without repacking.
* `takeWhile` and `takeWhile1` are about 150 times faster than approaches
  involving `many`, `manyTill` and other similar combinators.
* `takeP` allows us to grab n tokens from the stream and returns them as a
  “chunk” of the stream.

Megaparsec is about as fast as Attoparsec if you write your parser carefully
(see also [the section about performance](#performance)).

The library can currently work with the following types of input stream
out-of-the-box:

* `String = [Char]`
* `ByteString` (strict and lazy)
* `Text` (strict and lazy)

It's also possible to make it work with custom token streams by making them
an instance of the `Stream` type class.

### Error messages

Megaparsec has well-typed error messages and the ability to signal custom
parse errors to better work in user's domain of interest.

Megaparsec 7 introduced the `ParseErrorBundle` data type that helps to
manage multi-error messages and pretty-print them easily and efficiently.
That version of the library also made the practice of displaying offending
line the default, similar to how recent versions of GHC do it.

### Alex support

Megaparsec works well with streams of tokens produced by tools like Alex.
The design of the `Stream` type class has been changed significantly in
versions 6 and 7, but user can still work with custom streams of tokens
without problems.

### Character and binary parsing

Megaparsec has decent support for Unicode-aware character parsing. Functions
for character parsing live in the [`Text.Megaparsec.Char`][tm-char] module.
Similarly, there is [`Text.Megaparsec.Byte`][tm-byte] module for parsing
streams of bytes.

### Lexer

[`Text.Megaparsec.Char.Lexer`][tm-char-lexer] is a module that should help
you write your lexer. If you have used `Parsec` in the past, this module
“fixes” its particularly inflexible `Text.Parsec.Token`.

[`Text.Megaparsec.Char.Lexer`][tm-char-lexer] is intended to be imported
using a qualified import, it's not included in [`Text.Megaparsec`][tm]. The
module doesn't impose how you should write your parser, but certain
approaches may be more elegant than others. An especially important theme is
parsing of white space, comments, and indentation.

The design of the module allows one quickly solve simple tasks and doesn't
get in the way when the need to implement something less standard arises.

[`Text.Megaparsec.Byte.Lexer`][tm-byte-lexer] is also available for users
who wish to parse binary data.

## Documentation

Megaparsec is well-documented. See the [current version of Megaparsec
documentation on Hackage][hackage].

## Tutorials

You can find Megaparsec tutorials [here][tutorials]. They should provide
sufficient guidance to help you start with your parsing tasks. The site also
has instructions and tips for Parsec users who decide to migrate to
Megaparsec.

## Performance

Despite being flexible, Megaparsec is also fast. Here is how Megaparsec
7.0.0 compares to [Attoparsec][attoparsec] 0.13.2.2 (the fastest widely used
parsing library in the Haskell ecosystem):

Test case         | Execution time | Allocated | Max residency
------------------|---------------:|----------:|-------------:
CSV (Attoparsec)  |       76.50 μs |   397,784 |        10,544
CSV (Megaparsec)  |       64.69 μs |   352,408 |         9,104
Log (Attoparsec)  |       302.8 μs | 1,150,032 |        10,912
Log (Megaparsec)  |       337.8 μs | 1,246,496 |        10,912
JSON (Attoparsec) |       18.20 μs |   128,368 |         9,032
JSON (Megaparsec) |       25.45 μs |   203,824 |         9,176

The benchmarks were created to guide development of Megaparsec 6 and can be
found [here][parsers-bench].

If you think your Megaparsec parser is not efficient enough, take a look at
[these instructions][fast-parser].

## Comparison with other solutions

There are quite a few libraries that can be used for parsing in Haskell,
let's compare Megaparsec with some of them.

### Megaparsec vs Attoparsec

[Attoparsec][attoparsec] is another prominent Haskell library for parsing.
Although both libraries deal with parsing, it's usually easy to decide which
you will need in particular project:

* *Attoparsec* is sometimes faster but not that feature-rich. It should be
  used when you want to process large amounts of data where performance
  matters more than quality of error messages.

* *Megaparsec* is good for parsing of source code or other human-readable
  texts. It has better error messages and it's implemented as monad
  transformer.

So, if you work with something human-readable where size of input data is
moderate, just go with Megaparsec, otherwise Attoparsec may be a better
choice.

### Megaparsec vs Parsec

Since Megaparsec is a fork of [Parsec][parsec], we are bound to list the
main differences between the two libraries:

* Better error messages. Megaparsec has well-typed error messages and custom
  error messages.

* Megaparsec can show the line on which parse error happened as part of
  parse error. This makes it a lot easier to figure out where the error
  happened.

* Some quirks and “buggy features” (as well as plain bugs) of original
  Parsec are fixed. There is no undocumented surprising stuff in Megaparsec.

* Better support for Unicode parsing in [`Text.Megaparsec.Char`][tm-char].

* Megaparsec has more powerful combinators and can parse languages where
  indentation matters out-of-the-box.

* Better documentation.

* Megaparsec can recover from parse errors “on the fly” and continue
  parsing.

* Megaparsec allows us to conditionally process parse errors *inside your
  parser* before parsing is finished. In particular, it's possible to define
  regions in which parse errors, should they happen, will get a “context
  tag”, e.g. we could build a context stack like “in function definition
  foo”, “in expression x”, etc. This is not possible with Parsec.

* Megaparsec is faster and supports efficient operations on top of `tokens`,
  `takeWhileP`, `takeWhile1P`, `takeP` like Attoparsec.

If you want to see a detailed change log, `CHANGELOG.md` may be helpful.
Also see [this original announcement][original-announcement] for another
comparison.

### Megaparsec vs Trifecta

[Trifecta][trifecta] is another Haskell library featuring good error
messages. Some reasons one may question choice of Trifecta is his/her
parsing library:

* Complicated, doesn't have any tutorials available, and documentation
  doesn't help at all.

* Trifecta can parse `String` and `ByteString` natively, but not `Text`.

* Trifecta's error messages may be different with their own features, but
  certainly not as flexible as Megaparsec's error messages in the latest
  versions.

* Depends on `lens`. This means you'll pull in half of Hackage as transitive
  dependencies. Also if you're not into `lens` and would like to keep your
  code “vanilla”, you may not like the API.

[Idris][idris] has recently switched from Trifecta to Megaparsec which
allowed it to [have better error messages and fewer
dependencies][idris-testimony].

### Megaparsec vs Earley

[Earley][earley] is a newer library that allows us to safely (it your code
compiles, then it probably works) parse context-free grammars (CFG).
Megaparsec is a lower-level library compared to Earley, but there are still
enough reasons to choose it:

* Megaparsec is faster.

* Your grammar may be not context-free or you may want introduce some sort
  of state to the parsing process. Almost all non-trivial parsers require
  something of this sort. Even if your grammar is context-free, state may
  allow us to add some additional niceties. Earley does not support that.

* Megaparsec's error messages are more flexible allowing to include
  arbitrary data in them, return multiple error messages, mark regions that
  affect any error that happens in those regions, etc.

* The approach Earley uses differs from the conventional monadic parsing. If
  you work not alone, people you work with, especially beginners, will be
  much more productive with libraries taking more traditional path to
  parsing like Megaparsec.

In other words, Megaparsec is less safe but also more powerful.

## Related packages

The following packages are designed to be used with Megaparsec (open a PR if
you want to add something to the list):

* [`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec)—utilities
  for testing Megaparsec parsers with with
  [Hspec](https://hackage.haskell.org/package/hspec).
* [`cassava-megaparsec`](https://hackage.haskell.org/package/cassava-megaparsec)—Megaparsec
  parser of CSV files that plays nicely with
  [Cassava](https://hackage.haskell.org/package/cassava).
* [`tagsoup-megaparsec`](https://hackage.haskell.org/package/tagsoup-megaparsec)—a
  library for easily using
  [TagSoup](https://hackage.haskell.org/package/tagsoup) as a token type in
  Megaparsec.

## Prominent projects that use Megaparsec

Some prominent projects that use Megaparsec:

* [Idris](https://github.com/idris-lang/Idris-dev)—a general-purpose
  functional programming language with dependent types
* [Hledger](https://github.com/simonmichael/hledger)—an accounting tool
* [MMark](https://github.com/mmark-md/mmark)—strict markdown processor for
  writers
* [Stache](https://github.com/stackbuilders/stache)—Mustache templates for
  Haskell
* [Language Puppet](https://github.com/bartavelle/language-puppet)—library
  for manipulating Puppet manifests

## Links to announcements and blog posts

Here are some blog posts mainly announcing new features of the project and
describing what sort of things are now possible:

* [Megaparsec 7](https://markkarpov.com/post/megaparsec-7.html)
* [Evolution of error messages](https://markkarpov.com/post/evolution-of-error-messages.html)
* [A major upgrade to Megaparsec: more speed, more power](https://markkarpov.com/post/megaparsec-more-speed-more-power.html)
* [Latest additions to Megaparsec](https://markkarpov.com/post/latest-additions-to-megaparsec.html)
* [Announcing Megaparsec 5](https://markkarpov.com/post/announcing-megaparsec-5.html)
* [Megaparsec 4 and 5](https://markkarpov.com/post/megaparsec-4-and-5.html)
* [The original Megaparsec 4.0.0 announcement][original-announcement]

## Authors

The project was started and is currently maintained by Mark Karpov. You can
find the complete list of contributors in the `AUTHORS.md` file in the
official repository of the project. Thanks to all the people who propose
features and ideas, although they are not in `AUTHORS.md`, without them
Megaparsec would not be so good.

## Contribution

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/megaparsec/issues).

Pull requests are also welcome.

## License

Copyright © 2015–2019 Megaparsec contributors\
Copyright © 2007 Paolo Martini\
Copyright © 1999–2000 Daan Leijen

Distributed under FreeBSD license.

[hackage]: https://hackage.haskell.org/package/megaparsec
[tutorials]: https://markkarpov.com/learn-haskell.html#megaparsec-tutorials

[tm]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html
[tm-char]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html
[tm-byte]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte.html
[tm-char-lexer]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html
[tm-byte-lexer]: https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte-Lexer.html

[attoparsec]: https://hackage.haskell.org/package/attoparsec
[parsec]: https://hackage.haskell.org/package/parsec
[trifecta]: https://hackage.haskell.org/package/trifecta
[earley]: https://hackage.haskell.org/package/Earley
[idris]: https://www.idris-lang.org/
[idris-testimony]: https://twitter.com/edwinbrady/status/950084043282010117?s=09

[parsers-bench]: https://github.com/mrkkrp/parsers-bench
[fast-parser]: https://markkarpov.com/megaparsec/writing-a-fast-parser.html
[original-announcement]: https://mail.haskell.org/pipermail/haskell-cafe/2015-September/121530.html
