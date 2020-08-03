let
  myHaskellPackageOverlay = self: super: {
    myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
    random-fu     = super.haskell.lib.disableLibraryProfiling (hself.callPackage  ./random-fu { });
    random-source = super.haskell.lib.disableLibraryProfiling (hself.callPackage ./random-source { });
    rvar          = super.haskell.lib.disableLibraryProfiling (hself.callPackage ./rvar { });
      };
    };
  };

  nixpkgs = import <nixpkgs> { overlays = [ myHaskellPackageOverlay ]; };

in

nixpkgs.myHaskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.myHaskellPackages.random-source;
  random-fu     = nixpkgs.myHaskellPackages.random-fu;
}

