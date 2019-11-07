{ pkgs ? (import ./nix/nixpkgs)
, ghc ? "ghc865"
}:

let

  megaparsecSource = pkgs.lib.sourceByRegex ./. [
    "^CHANGELOG\.md$"
    "^LICENSE\.md$"
    "^README\.md$"
    "^Text.*$"
    "^bench.*$"
    "^megaparsec\.cabal$"
  ];

  megaparsecTestsSource = pkgs.lib.sourceByRegex ./megaparsec-tests [
    "^LICENSE\.md$"
    "^README\.md$"
    "^megaparsec-tests\.cabal$"
    "^src.*$"
    "^tests.*$"
  ];

  parsersBenchSource = pkgs.lib.sourceByRegex ./parsers-bench [
    "^README\.md$"
    "^parsers-bench\.cabal$"
    "^ParsersBench.*$"
    "^bench.*$"
    "^data.*$"
  ];

  doBenchmark = p:
    let targets = ["bench-speed" "bench-memory"];
        copying = pkgs.lib.concatMapStrings
          (t: "cp dist/build/${t}/${t} $out/bench/\n")
          targets;
    in pkgs.haskell.lib.doBenchmark
      (p.overrideAttrs (drv: {
        postInstall = ''
        mkdir -p $out/bench
        if test -d data/
        then
          mkdir -p $out/bench/data
          cp data/* $out/bench/data/
        fi
        ${copying}
        '';
      }));

  doJailbreak = pkgs.haskell.lib.doJailbreak;

  megaparsecOverlay = self: super: {
    "megaparsec" = doBenchmark
      (super.callCabal2nix "megaparsec" megaparsecSource { });
    "megaparsec-tests" =
      super.callCabal2nix "megaparsec-tests" megaparsecTestsSource { };
    # The ‘parser-combinators-tests’ package is a bit special because it
    # does not contain an executable nor a library, so its install phase
    # normally fails. We want to build it and run the tests anyway, so we
    # have to do these manipulations.
    "parser-combinators-tests" = doJailbreak (pkgs.haskell.lib.dontHaddock
      (patch
        (super.parser-combinators-tests.overrideAttrs (drv: {
          installPhase = "mkdir $out";
          broken = false;
        }))
        ./nix/patches/parser-combinators-tests.patch));
    "modern-uri" = patch (doBenchmark super.modern-uri) ./nix/patches/modern-uri.patch;
    "mmark" = doJailbreak (doBenchmark (patch super.mmark ./nix/patches/mmark.patch));
    "parsers-bench" = doJailbreak (doBenchmark
      (super.callCabal2nix "parsers-bench" parsersBenchSource { }));
    "hspec-megaparsec" = doJailbreak
      (patch super.hspec-megaparsec ./nix/patches/hspec-megaparsec.patch);
    "dhall" = doJailbreak
      (patch super.dhall ./nix/patches/dhall.patch);
    "idris" = doJailbreak
      (patch super.idris ./nix/patches/idris.patch);
    "hledger-lib" = doJailbreak
      (patch super.hledger-lib ./nix/patches/hledger-lib.patch);
    "hledger" = doJailbreak super.hledger;
    "cue-sheet" = doJailbreak super.cue-sheet;
    "cassava-megaparsec" = doJailbreak super.cassava-megaparsec;
    "language-puppet" = doJailbreak super.language-puppet;
    "stache" = doJailbreak super.stache;
    "tomland" = doJailbreak super.tomland;
  };

  updatedPkgs = pkgs // {
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        "${ghc}" = pkgs.haskell.packages.${ghc}.override {
          overrides = megaparsecOverlay;
        };
      };
    };
  };

  haskellPackages = updatedPkgs.haskell.packages.${ghc};

  patch = p: patch:
    pkgs.haskell.lib.appendPatch p patch;

in {
  # Base: Megaparsec and its unit tests:
  base = pkgs.recurseIntoAttrs {
    inherit (haskellPackages)
      hspec-megaparsec
      megaparsec
      megaparsec-tests
      parser-combinators-tests;
  };

  # Dependent packages of interest:
  deps = pkgs.recurseIntoAttrs {
    inherit (haskellPackages)
      cachix
      cassava-megaparsec
      cue-sheet
      dhall
      hledger
      hnix
      idris
      language-puppet
      mmark
      modern-uri
      replace-megaparsec
      stache
      tomland;
  };

  # Benchmarks:
  benches = pkgs.recurseIntoAttrs {
    inherit (haskellPackages)
      megaparsec
      mmark
      modern-uri
      parsers-bench;
  };

  # For development:
  shell = haskellPackages.shellFor {
    packages = ps: [
      ps.megaparsec
      ps.megaparsec-tests
    ];
    buildInputs = with haskellPackages; [
      cabal-install
      ghcid
    ];
  };

  # Distribution tarballs:
  dist = with pkgs.haskell.lib; pkgs.recurseIntoAttrs {
    megaparsec = sdistTarball haskellPackages.megaparsec;
    megaparsec-tests = sdistTarball haskellPackages.megaparsec-tests;
  };
}
