{ nixpkgs ? import <nixos-unstable> {}, compiler ? "ghc802"}:

{ arkmonoid = nixpkgs.haskell.packages.${compiler}.callPackage ./default.nix {};
}
