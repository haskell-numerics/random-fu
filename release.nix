let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskell.packages.ghc8107.override {
    overrides = hself: hsuper: rec {
      random-fu     =  hself.callPackage  ./random-fu { };
      rvar          =  hself.callPackage ./rvar { };
      microstache   = super.haskell.lib.doJailbreak hsuper.microstache;
    };
  };
};

# nixpkgs = import (builtins.fetchTarball {
#   url = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
#   sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
# }) { overlays = [ myHaskellPackageOverlay ]; };

in

{ nixpkgs ? import <nixpkgs> { config.allowBroken = true; overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:

nixpkgs.myHaskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.myHaskellPackages.random-source;
  random-fu     = nixpkgs.myHaskellPackages.random-fu;
}
