{ mkDerivation, base, MonadPrompt, mtl, random-source, stdenv
, transformers
}:
mkDerivation {
  pname = "rvar";
  version = "0.2.0.4";
  src = ./.;
  libraryHaskellDepends = [
    base MonadPrompt mtl random-source transformers
  ];
  homepage = "https://github.com/mokus0/random-fu";
  description = "Random Variables";
  license = stdenv.lib.licenses.publicDomain;
}
