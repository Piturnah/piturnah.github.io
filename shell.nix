{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      cabal-install
      ghc
      zlib
      haskellPackages.haskell-language-server
      haskellPackages.fourmolu
    ];
}
