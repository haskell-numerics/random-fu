let
  overlay = self: super:
{
  random-fu     = self.haskellPackages.callPackage ./random-fu { rvar = self.rvar; };
  random-source = self.haskellPackages.callPackage ./random-source { };
  rvar          = self.haskellPackages.callPackage ./rvar { random-source = self.random-source; };
};

in

nixpkgs.myHaskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.myHaskellPackages.random-source;
  random-fu     = nixpkgs.myHaskellPackages.random-fu;
}

