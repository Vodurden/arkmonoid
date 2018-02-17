{ mkDerivation, base, ecstasy, gloss, linear, stdenv, transformers
}:
mkDerivation {
  pname = "hblock";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base ecstasy gloss linear transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
