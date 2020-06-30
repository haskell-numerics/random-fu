let
  myHaskellPackageOverlay = self: super: {
    myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
        random = hsuper.random_1_2_0;
        mkDerivation = args: hsuper.mkDerivation (args // {
          doCheck = false;
          doHaddock = false;
        });

    random-fu     = super.haskell.lib.disableLibraryProfiling (hself.callPackage  ./random-fu { });
    random-source = super.haskell.lib.disableLibraryProfiling (hself.callPackage ./random-source { random = hsuper.random_1_2_0; });
    rvar          = super.haskell.lib.disableLibraryProfiling (hself.callPackage ./rvar { });
    splitmix      = hsuper.splitmix_0_1;
    MonadRandom   = hsuper.MonadRandom_0_5_2;
    };
  };
};

  nixpkgs = import <nixpkgs> { overlays = [ myHaskellPackageOverlay ]; };

in

nixpkgs.myHaskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.myHaskellPackages.random-source;
  random-fu     = nixpkgs.myHaskellPackages.random-fu;
}
