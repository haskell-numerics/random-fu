{ mkDerivation, base, flexible-defaults, mersenne-random-pure64
, mtl, mwc-random, primitive, random, stateref, stdenv, syb
, template-haskell, th-extras
}:
mkDerivation {
  pname = "random-source";
  version = "0.3.0.8";
  src = ./.;
  libraryHaskellDepends = [
    base flexible-defaults mersenne-random-pure64 mtl mwc-random
    primitive random stateref syb template-haskell th-extras
  ];
  homepage = "https://github.com/mokus0/random-fu";
  description = "Generic basis for random number generators";
  license = stdenv.lib.licenses.publicDomain;
}
