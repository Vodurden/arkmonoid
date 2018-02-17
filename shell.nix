{ nixpkgs ? import <nixos-unstable> {}, compiler ? "ghc802", doBenchmark ? false }:

let
  pkgs = nixpkgs;

  f = import ./default.nix;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = haskellNew: haskellOld: rec {
      # OpenGLRaw segfaults when we try to build it's haddocks
      OpenGLRaw = pkgs.haskell.lib.dontHaddock haskellOld.OpenGLRaw;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
in
  if pkgs.lib.inNixShell then drv.env else drv
