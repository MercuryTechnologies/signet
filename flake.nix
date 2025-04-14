{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        packages.default = pkgs.haskellPackages.callCabal2nix "signet" self {};
        devShells.default = pkgs.mkShell {
          buildInputs = [
            pkgs.haskellPackages.cabal-gild
            pkgs.cabal-install
            pkgs.ghc
            pkgs.hlint
            pkgs.ormolu
          ];
        };
      }
    );
}
