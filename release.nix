{ nixpkgs ? import <nixos-unstable> {}, compiler ? "ghc802"}:

{ hblock = nixpkgs.haskell.packages.${compiler}.callPackage ./default.nix {};
}
