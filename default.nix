let
  pkgs = import <nixpkgs> { };
in
pkgs.haskell.packages.ghc98.developPackage {
  root = ./.;
  modifier =
    drv:
    pkgs.haskell.lib.addBuildTools drv (
      with pkgs.haskell.packages.ghc98;
      [
        cabal-install
        cabal-gild
        hlint
        ormolu
      ]
    );
}
