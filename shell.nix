{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = [
    (haskellPackages.ghcWithPackages (ps: [
      ps.pretty-simple
      ps.first-class-families
    ]))
    haskellPackages.ghcid
    hlint
  ];
}
