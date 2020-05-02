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

{ nixpkgs ? import <nixpkgs> { overlays = [ overlay ]; } }:

nixpkgs.haskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.random-source;
  random-fu     = nixpkgs.random-fu;
}

