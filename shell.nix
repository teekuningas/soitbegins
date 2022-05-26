{ pkgs ? import <nixpkgs> {} }:
(pkgs.buildFHSUserEnv {
  name = "soitbegins-env";
  targetPkgs = pkgs: (with pkgs; [
    python39Full
    python39Packages.pip
    python39Packages.pyqt5
    python39Packages.vispy
    qt5Full
    freetype
    gcc
    glib
    zip
    nodejs
    zlib
    xorg.libX11
    xorg.libXrender
  ]);
  runScript = "bash prepare-nix.sh";
}).env
