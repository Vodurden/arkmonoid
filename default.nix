{ # Nix dependencies
  mkDerivation, darwin

  # Haskell lib dependencies
  , base, ecstasy, gloss, linear, stdenv, transformers, containers, lens

  # Haskell test dependencies
  , tasty, tasty-discover, tasty-hunit, HUnit, tasty-hedgehog, hedgehog

  # Extra executables
  , flamegraph, ghc-prof-flamegraph
}:
mkDerivation {
  pname = "arkmonoid";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [flamegraph ghc-prof-flamegraph] ++ (if stdenv.isDarwin then [darwin.apple_sdk.frameworks.OpenGL] else []);
  executableHaskellDepends = [
    base ecstasy gloss linear transformers containers lens
    tasty tasty-discover tasty-hunit HUnit tasty-hedgehog hedgehog
  ];
  license = stdenv.lib.licenses.bsd3;
}
