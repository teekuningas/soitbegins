{
  description = "frontend";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem 
      (system: 
        let pkgs = nixpkgs.legacyPackages.${system}; 
        in {
          devShell = import ./shell.nix { pkgs = pkgs; };
        }
      );
}


