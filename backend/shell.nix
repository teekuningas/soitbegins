{ pkgs ? import <nixpkgs> {} }:
(pkgs.buildFHSUserEnv {
  name = "soitbegins-env";
  targetPkgs = pkgs: (with pkgs; [
    python39Full
    python39Packages.pip
    python39Packages.pyqt5
    qt5Full
    gcc
    freetype
    glib
    zlib
    xorg.libX11
    xorg.libXrender
  ]);
  runScript = "bash prepare-nix.sh";
}).env
