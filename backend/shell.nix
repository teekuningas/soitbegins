{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    packages = [ 
      (pkgs.python39.withPackages (ps: [ ps.websockets ]))
   ];
  }
