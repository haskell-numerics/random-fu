{ mkDerivation, base, bytestring, lib, MonadPrompt, mtl, random
, transformers
}:
mkDerivation {
  pname = "rvar";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring MonadPrompt mtl random transformers
  ];
  homepage = "https://github.com/mokus0/random-fu";
  description = "Random Variables";
  license = lib.licenses.publicDomain;
}
