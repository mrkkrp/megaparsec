{ pkgs ? (import ./nix/nixpkgs) }:

(import ./default.nix { inherit pkgs; }).shell
