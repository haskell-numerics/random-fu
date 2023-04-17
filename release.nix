let

myHaskellPackageOverlay = self: super: {
  myHaskellPackages = super.haskell.packages.ghc961.override {
    overrides = hself: hsuper: rec {
      random-fu     =  hself.callPackage  ./random-fu { };
      random-source =  hself.callPackage ./random-source { };
      rvar          =  hself.callPackage ./rvar { };
      vector-binary-instances = super.haskell.lib.doJailbreak hsuper.vector-binary-instances;
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
