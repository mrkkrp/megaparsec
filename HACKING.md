# Hacking

* [Development with `ghcid`](#development-with-ghcid)
* [Running unit tests](#running-unit-tests)
* [Checking dependent packages](#checking-dependent-packages)
* [Benchmarks](#benchmarks)
* [Releasing a new version](#releasing-a-new-version)

This document tries to describe everything you need to know to
develop/maintain Megaparsec.

## Development with `ghcid`

We use `nix` for development. First enter the `nix-shell`:

```console
$ nix-shell
```

Inside the shell you can:

* Build the `megaparsec` and `megaparsec-tests` packages with `cabal
  new-build all`.

* Run tests from the `megaparsec-tests` package with `cabal new-test all`.

* Run `ghcid` for interactive feedback as you edit with `ghcid
  --command="cabal new-repl megaparsec"` or `ghcid --command="cabal new-repl
  megaparsec-tests --enable-tests"` depending on the package you're editing.

## Running unit tests

The tests in `megaparsec-tests` are usually not enough to gain confidence in
your changes. It is wise to use tests from other packages:

```console
$ nix-build -A base --no-out-link
```

`base` derivation includes building and testing the following packages:

* `megaparsec`
* `hspec-megaparsec`
* `megaparsec-tests`
* `parser-combinators-tests`

It is worth noting that individual derivations from `base` can be built like
this:

```console
$ nix-build -A base.parser-combinators-tests --no-out-link
```

## Checking dependent packages

To find out how your changes affect a selected set of dependent packages do:

```console
$ nix-build -A deps --no-out-link
```

The “selected set” includes packages that are known to be high-quality,
well-tested, and non-trivial, so they are good targets for this sort of
testing. You can also try to build and test a particular package like this:

```console
$ nix-build -A deps.mmark --no-out-link
```

When you introduce a breaking change, some packages may stop compiling.
Usually it's easy enough to create a patch and make it compile with the
current dev version of Megaparsec. To do so, follow these steps:

* Start by cloning the repo of the failing package.

* Checkout commit that corresponds to the version our current `nixpkgs`
  packages use.

* Attempt to compile the package with current dev version of Megaparsec to
  reproduce the build errors. Often, if the broken package uses stack you
  can just add path to the updated Megaparsec directory to the `extra-deps`
  section.

* Perform whatever changes that are necessary to make the package work.

* Use `git diff > my-package.patch` or `git diff --cached >
  my-package.patch` to create the patch file. The first command will output
  unstaged changes, while the second command will write only staged changes.

* Adjust `default.nix` to apply the patch. You want to edit the `deps`
  attribute set. For example:

  ```nix
  # Dependent packages of interest:
  deps = pkgs.recurseIntoAttrs {
    # ...
    idris = patch haskellPackages.idris ./nix/patches/idris.patch;
  };
  ```

## Benchmarks

To build all benchmarks run:

```console
$ nix-build -A benches
```

This will create several `result-*` symlinks with benchmarks. It is also
possible to build benchmarks for just a specific package:

```console
$ nix-build -A benches.megaparsec # builds megaparsec's microbenchmarks
```

`cd` to `result/bench` and run benchmarks from there because some benchmarks
need data to run on and the paths are relative, so it'll fail if run from
root of Megaparsec's repo.

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
  $ nix-build -A dist
  ```

  This will create two symlinks to directories containing the tarballs.
  Typically they are called `result` and `result-2`.

* To upload the tarballs to Hackage, execute the following:

  ```console
  $ cabal upload --publish result/megaparsec-*.tar.gz
  $ cabal upload --publish result-2/megaparsec-tests-*.tar.gz
  ```
