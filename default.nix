{ pkgs ? import <nixpkgs> {} }:

let
  hh-free = pkgs.haskellPackages.developPackage { root = ./.; };

in
  pkgs.lib.overrideDerivation hh-free (args: {
    nativeBuildInputs = args.nativeBuildInputs ++ [ pkgs.haskellPackages.cabal-install ];
  })
