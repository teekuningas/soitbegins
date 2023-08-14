{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pythonEnv = pkgs.python3.withPackages (ps: with ps; [
          websockets
          black
        ]);
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ pythonEnv ];
        };

        packages = {
          dockerImage = pkgs.dockerTools.buildImage {
            name = "soitbegins-backend";
            tag = "latest";
            runAsRoot = ''
              cp -r ${./.} /code
              chmod -R +w /code
            '';
            config = {
              Cmd = [ "python3" "server.py" ];
              WorkingDir = "/code";
            };
            copyToRoot = with pkgs; [ pythonEnv bashInteractive curl coreutils ];
          };
        };
      }
    );
}

