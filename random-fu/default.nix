{ mkDerivation, base, erf, lib, math-functions, monad-loops, mtl
, random, random-shuffle, rvar, syb, template-haskell, transformers
, vector
}:
mkDerivation {
  pname = "random-fu";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base erf math-functions monad-loops mtl random random-shuffle rvar
    syb template-haskell transformers vector
  ];
  homepage = "https://github.com/mokus0/random-fu";
  description = "Random number generation";
  license = lib.licenses.publicDomain;
}
