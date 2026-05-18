{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pythonEnv = pkgs.python3.withPackages (ps: with ps; [
          websockets
          pyzmq
          black
        ]);
        pythonProd = pkgs.python3.withPackages (ps: with ps; [
          websockets
          pyzmq
        ]);
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [ pythonEnv ];
        };

        packages = {
          dockerImage = pkgs.dockerTools.buildImage {
            name = "soitbegins-server";
            tag = "latest";
            runAsRoot = ''
              cp -r ${./.} /code
              chmod -R +w /code
            '';
            config = {
              Cmd = [ "${pythonProd}/bin/python3" "server.py" ];
              WorkingDir = "/code";
              Env = [ "PATH=${pythonProd}/bin:${pkgs.coreutils}/bin" ];
            };
            copyToRoot = with pkgs; [ pythonProd coreutils ];
          };
        };
      }
    );
}

