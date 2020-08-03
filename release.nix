let
  overlay = self: super:
{
  random-fu     = self.haskellPackages.callPackage ./random-fu { rvar = self.rvar; };
  random-source = self.haskellPackages.callPackage ./random-source { };
  rvar          = self.haskellPackages.callPackage ./rvar { random-source = self.random-source; };
};

in

{ nixpkgs ? import <nixpkgs> { overlays = [ overlay ]; } }:

nixpkgs.haskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.random-source;
  random-fu     = nixpkgs.random-fu;
}

