{ mkDerivation, base, ecstasy, gloss, linear, stdenv, transformers, darwin, containers, flamegraph, ghc-prof-flamegraph, profiteur
}:
mkDerivation {
  pname = "arkmonoid";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [profiteur flamegraph ghc-prof-flamegraph] ++ (if stdenv.isDarwin then [darwin.apple_sdk.frameworks.OpenGL] else []);
  executableHaskellDepends = [
    base ecstasy gloss linear transformers containers
  ];
  license = stdenv.lib.licenses.bsd3;
}
