{
  description = "Megaparsec Nix helpers";
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
    flake-utils = {
      type = "github";
      owner = "numtide";
      repo = "flake-utils";
    };
  };
  outputs = { self, nixpkgs, flake-utils }:
    let
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        config.allowBroken = true;
      };
      ghc = "ghc924";

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
        let
          targets = [ "bench-speed" "bench-memory" ];
          copying = pkgs.lib.concatMapStrings
            (t: "cp dist/build/${t}/${t} $out/bench/\n")
            targets;
        in
        pkgs.haskell.lib.doBenchmark
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

      patch = p: patch:
        pkgs.haskell.lib.appendPatch p patch;

      megaparsecOverlay = self: super: {
        "megaparsec" = doBenchmark
          (super.callCabal2nix "megaparsec" megaparsecSource { });
        "megaparsec-tests" =
          super.callCabal2nix "megaparsec-tests" megaparsecTestsSource { };
        # The ‘parser-combinators-tests’ package is a bit special because it
        # does not contain an executable nor a library, so its install phase
        # normally fails. We want to build it and run the tests anyway, so we
        # have to do these manipulations.
        "parser-combinators-tests" = pkgs.haskell.lib.dontHaddock
          (super.parser-combinators-tests.overrideAttrs (drv: {
            installPhase = "mkdir $out";
          }));
        "modern-uri" = doBenchmark super.modern-uri;
        "parsers-bench" = doBenchmark
          (super.callCabal2nix "parsers-bench" parsersBenchSource { });
        "mmark" = doBenchmark super.mmark;
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

      # Base: Megaparsec and its unit tests:
      base = {
        inherit (haskellPackages)
          hspec-megaparsec
          megaparsec
          megaparsec-tests
          parser-combinators-tests;
      };

      # Dependent packages of interest:
      deps = {
        inherit (haskellPackages)
          cachix
          cassava-megaparsec
          cue-sheet
          dhall
          hledger
          idris
          mmark
          modern-uri
          replace-megaparsec
          stache
          tomland;
      };

      # Benchmarks:
      benches = {
        inherit (haskellPackages)
          megaparsec
          mmark
          modern-uri
          parsers-bench;
      };

      # Source distributions:
      dist = with pkgs.haskell.lib; {
        megaparsec = sdistTarball haskellPackages.megaparsec;
        megaparsec-tests = sdistTarball haskellPackages.megaparsec-tests;
      };

    in
    flake-utils.lib.eachDefaultSystem (system:
      {
        packages = flake-utils.lib.flattenTree {
          base = pkgs.recurseIntoAttrs base;
          all_base = pkgs.linkFarmFromDrvs "base" (builtins.attrValues base);
          deps = pkgs.recurseIntoAttrs deps;
          all_deps = pkgs.linkFarmFromDrvs "deps" (builtins.attrValues deps);
          benches = pkgs.recurseIntoAttrs benches;
          all_benches = pkgs.linkFarmFromDrvs "benches" (builtins.attrValues benches);
          dist = pkgs.recurseIntoAttrs dist;
          all_dist = pkgs.linkFarmFromDrvs "dist" (builtins.attrValues dist);
        };
        defaultPackage = base.megaparsec;
        devShells.default = haskellPackages.shellFor {
          packages = ps: [
            ps.megaparsec
            ps.megaparsec-tests
          ];
          buildInputs = with haskellPackages; [
            cabal-install
            ghcid
          ];
        };
      });
}
