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
    * [Megaparsec vs Attoparsec](#megaparsec-vs-attoparsec)
    * [Megaparsec vs Parsec](#megaparsec-vs-parsec)
    * [Megaparsec vs Trifecta](#megaparsec-vs-trifecta)
    * [Megaparsec vs Earley](#megaparsec-vs-earley)
    * [Megaparsec vs Parsers](#megaparsec-vs-parsers)
* [Related packages](#related-packages)
* [Links to announcements](#links-to-announcements)
* [Authors](#authors)
* [Contribution](#contribution)
* [License](#license)

This is an industrial-strength monadic parser combinator library. Megaparsec
is a fork of [Parsec](https://github.com/aslatter/parsec) library originally
written by Daan Leijen.

## Features

This project provides flexible solutions to satisfy common parsing needs.
The section describes them shortly. If you're looking for comprehensive
documentation, see the [section about documentation](#documentation).

### Core features

The package is built around `MonadParsec`, an MTL-style monad transformer.
All tools and features work with all instances of `MonadParsec`. You can
achieve various effects combining monad transformers, i.e. building monad
stack. Since the standard common monad transformers like `WriterT`,
`StateT`, `ReaderT` and others are instances of the `MonadParsec` type
class, you can wrap `ParsecT` *in* these monads, achieving, for example,
backtracking state.

On the other hand `ParsecT` is an instance of many type classes as well. The
most useful ones are `Monad`, `Applicative`, `Alternative`, and
`MonadParsec`.

The module
[`Text.Megaparsec.Combinator`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Combinator.html) (its
functions are included in `Text.Megaparsec`) contains traditional, general
combinators that work with instances of `Applicative` and `Alternative`.

Let's enumerate methods of the `MonadParsec` type class. The class abstracts
primitive functions of Megaparsec parsing. The rest of the library is built
via combination of these primitives:

* `failure` allows to fail reporting an arbitrary parse error.

* `label` allows to add a “label” to a parser, so if it fails the user will
  see the label instead of an automatically deduced expected token.

* `hidden` hides a parser from error messages altogether. This is the
  recommended way to hide things, prefer it to the `label ""` approach.

* `try` enables backtracking in parsing.

* `lookAhead` allows to parse input without consuming it.

* `notFollowedBy` succeeds when its argument fails and does not consume
  input.

* `withRecovery` allows to recover from parse errors “on-the-fly” and
  continue parsing. Once parsing is finished, several parse errors may be
  reported or ignored altogether.

* `observing` allows to “observe” parse errors without ending parsing (they
  are returned in `Left`, while normal results are wrapped in `Right`).

* `eof` only succeeds at the end of input.

* `token` is used to parse a single token.

* `tokens` makes it easy to parse several tokens in a row.

* `getParserState` returns the full parser state.

* `updateParserState` applies a given function on the parser state.

This list of core functions is longer than in some other libraries. Our goal
is efficient, readable implementations, and rich functionality, not minimal
number of primitive combinators. You can read the comprehensive description
of every primitive function in
[Megaparsec documentation](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Prim.html).

Megaparsec can currently work with the following types of input stream
out-of-the-box:

* `String` = `[Char]`
* `ByteString` (strict and lazy)
* `Text` (strict and lazy)

It's also simple to make it work with custom token streams, and Megaparsec
users have done so many times with great success.

### Error messages

Megaparsec 5 introduces well-typed error messages and the ability to use
custom data types to adjust the library to specific domain of interest. No
need to use a shapeless bunch of strings anymore.

The default error component (`Dec`) has constructors corresponding to the
`fail` function and indentation-related error messages. It is a decent
option that should work out-of-box for most parsing needs, while you are
free to use your own custom error component when necessary.

This new design allowed Megaparsec 5 to have much more helpful error
messages for indentation-sensitive parsing instead of the plain “incorrect
indentation” phrase.

### Alex and Happy support

Megaparsec works well with streams of tokens produced by tools like
Alex/Happy. Megaparsec 5 adds `updatePos` method to `Stream` type class that
gives you full control over textual positions that are used to report token
positions in error messages. You can update current position on per
character basis or extract it from token.

### Character parsing

Megaparsec has decent support for Unicode-aware character parsing. Functions
for character parsing live in the
[`Text.Megaparsec.Char`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html)
module (they all are included in `Text.Megaparsec`). The functions can be
divided into several categories:

* *Simple parsers*—parsers that parse certain character or several
  characters of the same kind. This includes `newline`, `crlf`, `eol`,
  `tab`, and `space`.

* *Parsers corresponding to categories of characters* parse single character
  that belongs to certain category of characters, for example:
  `controlChar`, `spaceChar`, `upperChar`, `lowerChar`, `printChar`,
  `digitChar`, and others.

* *General parsers* that allow you to parse a single character you specify
  or one of the given characters, or any character except for the given
  ones, or character satisfying given predicate. Case-insensitive versions
  of the parsers are available.

* *Parsers for sequences of characters* parse strings. Case-sensitive
  `string` parser is available as well as case-insensitive `string'`.

### Permutation parsing

For those who are interested in parsing of permutation phrases, there
is [`Text.Megaparsec.Perm`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Perm.html).
You have to import the module explicitly, it's not included in the
`Text.Megaparsec` module.

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

`Text.Megaparsec.Lexer` is intended to be imported via a qualified import,
it's not included in `Text.Megaparsec`. The module doesn't impose how you
should write your parser, but certain approaches may be more elegant than
others. An especially important theme is parsing of white space, comments,
and indentation.

The design of the module allows you quickly solve simple tasks and doesn't
get in your way when you want to implement something less standard.

Since Megaparsec 5, all tools for indentation-sensitive parsing are
available in `Text.Megaparsec.Lexer` module—no third party packages
required.

## Documentation

Megaparsec is well-documented. All functions and data-types are thoroughly
described. We pay attention to avoid outdated info or unclear phrases in our
documentation. See the [current version of Megaparsec documentation on
Hackage](https://hackage.haskell.org/package/megaparsec) for yourself.

## Tutorials

You can find Megaparsec tutorials
[here](https://markkarpov.com/learn-haskell.html#megaparsec-tutorials). They
should provide sufficient guidance to help you to start with your parsing
tasks. The site also has instructions and tips for Parsec users who decide
to migrate to Megaparsec.

## Performance

Despite being quite flexible, Megaparsec is also faster than Parsec. The
repository includes benchmarks that can be easily used to compare Megaparsec
and Parsec. In most cases Megaparsec is faster, sometimes dramatically
faster. If you happen to have some other benchmarks, I would appreciate if
you add Megaparsec to them and let me know how it performs.

If you think your Megaparsec parser is not efficient enough, take a look
at [these instructions](https://markkarpov.com/megaparsec/writing-a-fast-parser.html).

## Comparison with other solutions

There are quite a few libraries that can be used for parsing in Haskell,
let's compare Megaparsec with some of them.

### Megaparsec vs Attoparsec

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

### Megaparsec vs Parsec

Since Megaparsec is a fork of Parsec, we are bound to list the main
differences between the two libraries:

* Better error messages. We test our error messages using dense QuickCheck
  tests. Good error messages are just as important for us as correct return
  values of our parsers. Megaparsec will be especially useful if you write a
  compiler or an interpreter for some language.

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

* Megaparsec allows to conditionally process parse errors *inside your
  parser* before parsing is finished. In particular, it's possible to define
  regions in which parse errors, should they happen, will get a “context
  tag”, e.g. we could build a context stack like “in function definition
  foo”, “in expression x”, etc. This is not possible with Parsec.

* Megaparsec is faster.

* Megaparsec is ~~better~~ supported.

If you want to see a detailed change log, `CHANGELOG.md` may be helpful.
Also see [this original announcement](https://notehub.org/w7037) for another
comparison.

To be honest Parsec's development has seemingly stagnated. It has no test
suite (only three per-bug tests), and all its releases beginning from
version 3.1.2 (according or its change log) were about introducing and
fixing regressions. Parsec is old and somewhat famous in the Haskell
community, so we understand there will be some kind of inertia, but we
advise you use Megaparsec from now on because it solves many problems of the
original Parsec project. If you think you still have a reason to use
original Parsec, open an issue.

### Megaparsec vs Trifecta

[Trifecta](https://hackage.haskell.org/package/trifecta) is another Haskell
library featuring good error messages. Like some other projects of Edward
Kmett, it's probably good, but also under-documented, and has
unfixed [bugs and flaws](https://github.com/ekmett/trifecta/issues) that
Edward is too busy to fix (simply a fact, no offense intended). Other
reasons one may question choice of Trifecta is his/her parsing library:

* Complicated, doesn't have any tutorials available, and documentation
  doesn't help at all.

* Trifecta can parse `String` and `ByteString` natively, but not `Text`.

* Trifecta's error messages may be different with their own features, but
  certainly not as flexible as Megaparsec's error messages in the latest
  versions.

* Depends on `lens`. This means you'll pull in half of Hackage as transitive
  dependencies. Also if you're not into `lens` and would like to keep your
  code “vanilla”, you may not like the API.

### Megaparsec vs Earley

[Earley](https://hackage.haskell.org/package/Earley) is a newer library that
allows to safely (it your code compiles, then it probably works) parse
context-free grammars (CFG). Megaparsec is a lower-level library compared to
Earley, but there are still enough reasons to choose it over Earley:

* Megaparsec is faster.

* Your grammar may be not context-free or you may want introduce some sort
  of state to the parsing process. Almost all non-trivial parsers require
  something of this sort. Even if your grammar is context-free, state may
  allow to add some additional niceties. Earley does not support that.

* Megaparsec's error messages are more flexible allowing to include
  arbitrary data in them, return multiple error messages, mark regions that
  affect any error that happens in those regions, etc.

* The approach Earley uses differs from the conventional monadic parsing. If
  you work not alone, chances people you work with, especially beginners
  will be much more productive with libraries taking more traditional path
  to parsing like Megaparsec.

IOW, Megaparsec is less safe but also more powerful.

### Megaparsec vs Parsers

There is [Parsers](https://hackage.haskell.org/package/parsers) package,
which is great. You can use it with Megaparsec or Parsec, but consider the
following:

* It depends on Attoparsec, Parsec, and Trifecta, which means you always
  grab half of Hackage as transitive dependencies by using it. This is
  ridiculous, by the way, because this package is supposed to be useful for
  parser builders, so they can write basic core functionality and get the
  rest “for free”.

* It currently has a ~~bug~~ feature in definition of `lookAhead` for
  various monad transformers like `StateT`, etc. which is visible when you
  create backtracking state via monad stack, not via built-in features. The
  feature makes it so `lookAhead` will backtrack your parser state but not
  your custom state added via `StateT`. Kmett thinks this behavior is
  better.

We intended to use Parsers library in Megaparsec at some point, but aside
from already mentioned flaws the library has different conventions for
naming of things, different set of “core” functions, etc., different
approach to lexing. So it didn't happen, Megaparsec has minimal
dependencies, it is feature-rich and self-contained.

## Related packages

The following packages are designed to be used with Megaparsec:

* [`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec)—utilities
  for testing Megaparsec parsers with
  with [Hspec](https://hackage.haskell.org/package/hspec).

* [`cassava-megaparsec`](https://hackage.haskell.org/package/cassava-megaparsec)—Megaparsec
  parser of CSV files that plays nicely
  with [Cassava](https://hackage.haskell.org/package/cassava).

* [`tagsoup-megaparsec`](https://hackage.haskell.org/package/tagsoup-megaparsec)—a
  library for easily
  using [TagSoup](https://hackage.haskell.org/package/tagsoup) as a token
  type in Megaparsec.

## Links to announcements

Here are some blog posts mainly announcing new features of the project and
describing what sort of things are now possible:

* [Latest additions to Megaparsec](https://markkarpov.com/post/latest-additions-to-megaparsec.html)
* [Announcing Megaparsec 5](https://markkarpov.com/post/announcing-megaparsec-5.html)
* [Megaparsec 4 and 5](https://markkarpov.com/post/megaparsec-4-and-5.html)
* [The original Megaparsec 4.0.0 announcement](https://notehub.org/w7037)

## Authors

The project was started and is currently maintained by Mark Karpov. You can
find the complete list of contributors in the `AUTHORS.md` file in the
official repository of the project. Thanks to all the people who propose
features and ideas, although they are not in `AUTHORS.md`, without them
Megaparsec would not be that good.

## Contribution

Issues (bugs, feature requests or otherwise feedback) may be reported in
[the GitHub issue tracker for this project](https://github.com/mrkkrp/megaparsec/issues).

Pull requests are also welcome (and yes, they will get attention and will be
merged quickly if they are good).

## License

Copyright © 2015–2017 Megaparsec contributors<br>
Copyright © 2007 Paolo Martini<br>
Copyright © 1999–2000 Daan Leijen

Distributed under FreeBSD license.
