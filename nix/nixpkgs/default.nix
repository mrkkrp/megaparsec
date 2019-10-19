let
  rev = "8bf142e001b6876b021c8ee90c2c7cec385fe8e9";
  sha256 = "1z8id8ix24ds9i7cm8g33v54j7xbhcpwzw50wlq00alj00xrq307";
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import nixpkgs {
    config.allowBroken = true;
    config.allowUnfree = true;
  };
in pkgs
