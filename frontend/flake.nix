{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          packages = [
            pkgs.nodejs_22
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
          ];
          # Native deps for node-gyp (lmdb, parcel native modules)
          buildInputs = [ pkgs.python3 ];
          nativeBuildInputs = [ pkgs.gcc pkgs.gnumake ];
        };
      }
    );
}
