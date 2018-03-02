{ mkDerivation, base, ecstasy, gloss, linear, stdenv, transformers, darwin
}:
mkDerivation {
  pname = "hblock";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = (if stdenv.isDarwin then [ darwin.apple_sdk.frameworks.OpenGL ] else []);
  executableHaskellDepends = [
    base ecstasy gloss linear transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
