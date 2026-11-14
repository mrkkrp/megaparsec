# Hacking

* [Building and testing](#building-and-testing)
* [Checking dependent packages](#checking-dependent-packages)
* [Benchmarks](#benchmarks)
* [Regenerating the Unicode tables](#regenerating-the-unicode-tables)
* [Releasing a new version](#releasing-a-new-version)

This document tries to describe everything you need to know to
develop/maintain Megaparsec.

## Building and testing

The repository is a `cabal` project that consists of three packages:
`megaparsec` itself, `megaparsec-tests` with its test suite, and
`parsers-bench` with benchmarks comparing Megaparsec to Attoparsec. To
build everything and run the tests:

```console
$ cabal build all
$ cabal test all
```

`cabal.project` enables the `dev` flag of the packages, which turns on
additional warnings and makes them errors.

## Checking dependent packages

The tests in `megaparsec-tests` are usually not enough to gain confidence in
non-trivial changes. It is wise to also build and test packages that depend
on Megaparsec against the working copy. For example, to do so with `mmark`,
unpack its source:

```console
$ cabal get mmark
```

and add its directory to `cabal.project.local`:

```cabal
packages: mmark-*/
```

Then `cabal test pkg:mmark` will build and test it with the version of
Megaparsec from the working copy. If the working copy has a major version
that the package does not accept yet, also add `allow-newer: megaparsec` to
`cabal.project.local`.

Good targets for this sort of testing are packages that are known to be
high-quality, well-tested, and non-trivial: `hspec-megaparsec`,
`parser-combinators-tests`, `cachix`, `cassava-megaparsec`, `cue-sheet`,
`dhall`, `hledger`, `idris`, `mmark`, `modern-uri`, `replace-megaparsec`,
`stache`, and `tomland`.

When you introduce a breaking change, some packages may stop compiling.
Usually it's easy enough to make them compile with the current dev version
of Megaparsec by editing the unpacked source.

## Benchmarks

Both `megaparsec` and `parsers-bench` have `bench-speed` and
`bench-memory` benchmarks:

```console
$ cabal bench pkg:megaparsec -O2
$ cabal bench pkg:parsers-bench -O2
```

Pass `-O2` because the `dev` flag that `cabal.project` enables drops the
`-O2` that the packages are normally compiled with.

## Regenerating the Unicode tables

`Text.Megaparsec.Unicode.Tables` holds the character width data that decides
how many columns a character takes up in a parse error, and it is generated
from the Unicode Character Database. Do not edit it by hand. To move to a
newer version of the standard, run the following from the root of the repo
and commit the result along with a changelog entry:

```console
$ runghc script/GenUnicodeTables.hs 17.0.0 > Text/Megaparsec/Unicode/Tables.hs
```

The script needs `curl` and downloads the two files it needs from
`unicode.org`, caching them in the working directory as `ucd-*.txt`. If you
already have a copy of the UCD, point the script at it instead with
`--ucd-dir DIR`; the directory should contain `EastAsianWidth.txt` and
`DerivedGeneralCategory.txt`. Omitting the version argument uses the version
that the script was last updated for, which is also recorded in
`unicodeVersion` in the generated module.

The hand-written code in `Text.Megaparsec.Unicode` decides the width of the
characters below the top of the Latin-1 range without consulting the tables
at all, since that is by far the most common case. The script checks that
this shortcut still agrees with the data and refuses to generate anything if
it does not, in which case both it and `charLength` need to be revisited.

## Releasing a new version

To release a new version of Megaparsec, follow these steps:

* Bump version for both `megaparsec` and `megaparsec-tests`. In the
  `megaparsec.cabal` file update the version of the package. In the
  `megaparsec-tests.cabal` file update the version of the package (we keep
  it in sync with version of `megaparsec`) as well as the versions of
  `megaparsec` it depends on.

* Create git tag and push it to the repo.

* Generate distribution tarballs by running:

  ```console
  $ cabal sdist pkg:megaparsec pkg:megaparsec-tests
  ```

  This will write `megaparsec-X.Y.Z.tar.gz` and
  `megaparsec-tests-X.Y.Z.tar.gz` to `dist-newstyle/sdist/`.

* To upload the tarballs to Hackage, execute the following:

  ```console
  $ cabal upload --publish dist-newstyle/sdist/megaparsec-X.Y.Z.tar.gz
  $ cabal upload --publish dist-newstyle/sdist/megaparsec-tests-X.Y.Z.tar.gz
  ```
