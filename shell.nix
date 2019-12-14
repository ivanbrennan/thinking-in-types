{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: [ps.pretty-simple]))
    haskellPackages.ghcid
    hlint
  ];
}
