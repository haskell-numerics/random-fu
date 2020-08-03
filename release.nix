let
  myHaskellPackageOverlay = self: super: {
    myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
        mkDerivation = args: hsuper.mkDerivation (args // {
          enableLibraryProfiling = false;
          doHaddock = false;
        });
    random-fu     = hself.callPackage ./random-fu { };
    random-source = hself.callPackage ./random-source { };
    rvar          = hself.callPackage ./rvar { };
      };
    };
  };

  nixpkgs = import <nixpkgs> { overlays = [ myHaskellPackageOverlay ]; };

in

nixpkgs.myHaskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.myHaskellPackages.random-source;
  random-fu     = nixpkgs.myHaskellPackages.random-fu;
}

