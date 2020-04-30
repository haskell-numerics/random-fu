{ mkDerivation, base, criterion, deepseq, mersenne-random-pure64
, MonadRandom, mtl, mwc-random, random, random-fu, random-source
, stateref, stdenv, vector
}:
mkDerivation {
  pname = "speed-tests";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base criterion deepseq mersenne-random-pure64 MonadRandom mtl
    mwc-random random random-fu random-source stateref vector
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
