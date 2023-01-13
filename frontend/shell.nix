{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    packages = [ pkgs.nodejs pkgs.elmPackages.elm ];
  }
