{ mkDerivation, base, erf, math-functions, monad-loops, mtl
, random-shuffle, random-source, rvar, stdenv, syb
, template-haskell, transformers, vector
}:
mkDerivation {
  pname = "random-fu";
  version = "0.2.7.4";
  src = ./.;
  libraryHaskellDepends = [
    base erf math-functions monad-loops mtl random-shuffle
    random-source rvar syb template-haskell transformers vector
  ];
  homepage = "https://github.com/mokus0/random-fu";
  description = "Random number generation";
  license = stdenv.lib.licenses.publicDomain;
}
