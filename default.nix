let
  pkgs = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-24.11.tar.gz") {};
in
pkgs.haskellPackages.developPackage {
  root = ./.;
  modifier =
    drv:
    pkgs.haskell.lib.addBuildTools drv (
      with pkgs.haskellPackages;
      [
        cabal-install
        cabal-gild
        haskell-language-server
        hlint
        ormolu
      ]
    );
}
