{ mkDerivation, base, criterion, deepseq, lib, mersenne-random-pure64
, MonadRandom, mtl, mwc-random, random, random-fu, random-source
, stateref, vector
}:
mkDerivation {
  pname = "speed-tests";
  version = "0.0.0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base criterion deepseq mersenne-random-pure64 MonadRandom mtl
    mwc-random random random-fu random-source stateref vector
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
