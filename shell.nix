{ compiler ? "ghc8107"
, nixpkgs ? <nixpkgs> }:
let
  config = {
    allowBroken = true;
  };

  pkgs = (import nixpkgs) { inherit config; };

  hpkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = (import ./nix/overrides.nix) pkgs;
  };
in
pkgs.mkShell {
  name = "clash-compiler-shell";
  buildInputs = [
    # For nix dependency management
    pkgs.niv

    # For quick clash experimentation
    (hpkgs.ghcWithPackages (p: with p; [
      clash-ghc

      ghc-typelits-extra
      ghc-typelits-knownnat
      ghc-typelits-natnormalise
    ])
    )
  ];
}
