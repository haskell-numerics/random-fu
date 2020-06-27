let
  myHaskellPackageOverlay = self: super: {
    myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {
    random-fu     = super.haskell.lib.disableLibraryProfiling (hself.callPackage  ./random-fu { });
    random-source = super.haskell.lib.disableLibraryProfiling (hself.callPackage ./random-source { });
    rvar          = super.haskell.lib.disableLibraryProfiling (hself.callPackage ./rvar { });

    # random =
    #     let newRandomSrc = builtins.fetchGit {
    #       url = "https://github.com/idontgetoutmuch/random.git";
    #       rev = "8ac2c89c8394555f56ade4eda34051599833e885";
    #       ref = "v1.2-proposal";
    #       };
    #         ran = hself.callCabal2nix "random" newRandomSrc {};
    #       in
    #       super.haskell.lib.dontCheck ran;

      };
    };
  };

  nixpkgs = import <nixpkgs> { overlays = [ myHaskellPackageOverlay ]; };

in

nixpkgs.myHaskellPackages.callPackage ./tests/speed {
  random-source = nixpkgs.myHaskellPackages.random-source;
  random-fu     = nixpkgs.myHaskellPackages.random-fu;
  # rvar          = nixpkgs.myHaskellPackages.rvar;
  # random        = nixpkgs.myHaskellPackages.random;
}

