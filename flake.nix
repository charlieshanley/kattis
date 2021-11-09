{
  description = "Shell for solving kattis problems";

  inputs.nixpkgs.url = "nixpkgs/nixos-21.05";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      ghc = pkgs.haskellPackages.ghcWithPackages (ps: [
        ps.QuickCheck
      ]);
      inherit (pkgs) callPackage mkShell ghcid cabal-install cabal2nix;
    in
      {
        devShell = mkShell {
          nativeBuildInputs = [
            ghc ghcid cabal-install cabal2nix
          ];
        };
      });
}
