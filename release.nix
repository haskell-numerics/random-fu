let
  myHaskellPackageOverlay = self: super: {
    myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
        random = hsuper.random_1_2_0;
        mkDerivation = args: hsuper.mkDerivation (args // {
          doCheck = false;
          doHaddock = false;
        });

    random-fu     =  hself.callPackage  ./random-fu { };
    random-source =  hself.callPackage ./random-source { random = hsuper.random_1_2_0; };
    rvar          =  hself.callPackage ./rvar { };
    splitmix      = hsuper.splitmix_0_1;
    MonadRandom   = hsuper.MonadRandom_0_5_2;
    };
  };
};

  nixpkgs = import (builtins.fetchTarball {
    name = "nixos-unstable-2018-06-28";
    url = "https://github.com/nixos/nixpkgs/archive/056b0df2b614893b15dd696e74d093f6c58590a0.tar.gz";
    sha256 = "1zjrjk5pfzl1nsii90q1v43cf55jr66igapa8pqgxdg465p186c2";
}) { overlays = [ myHaskellPackageOverlay ]; };

in

nixpkgs.myHaskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.myHaskellPackages.random-source;
  random-fu     = nixpkgs.myHaskellPackages.random-fu;
}
