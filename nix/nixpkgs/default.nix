let
  rev = "55beed9922c2f6b030af61ca7e33bd47850c68f2";
  sha256 = "0jxkb3bl7axa6vmfsfdfx4mxv6wx0pc8iiwgrw2qh8wxhlhbylks";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs {
    config.allowBroken = true;
    config.allowUnfree = true;
  };
in pkgs
