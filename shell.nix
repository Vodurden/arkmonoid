{ nixpkgs ? import <nixos-unstable> {}, compiler ? "ghc802", doBenchmark ? false }:

let
  pkgs = nixpkgs;

  f = import ./default.nix;

  haskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = haskellNew: haskellOld: rec {
      # We want to enable profiling for all our dependencies in nix-shell
      # so we can profile stuff!
      mkDerivation = args: haskellOld.mkDerivation (args // {
        enableLibraryProfiling = true;
      });

      # OpenGLRaw segfaults when we try to build it's haddocks
      OpenGLRaw = pkgs.haskell.lib.dontHaddock haskellOld.OpenGLRaw;

      # these tests take _forever_ 
      linear = pkgs.haskell.lib.dontCheck haskellOld.linear;
      happy = pkgs.haskell.lib.dontCheck haskellOld.happy;
    };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
in
  if pkgs.lib.inNixShell then drv.env else drv
