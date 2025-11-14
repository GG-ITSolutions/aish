{
  description = "AIsh - Agentic Terminal Assistant";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    pyproject-nix.url = "github:pyproject-nix/pyproject.nix";
    pyproject-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, pyproject-nix }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    in {
      packages = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          python = pkgs.python313;
          
          project = pyproject-nix.lib.project.loadPyproject {
            projectRoot = ./.;
          };
          
          attrs = project.renderers.buildPythonPackage { inherit python; };
        in {
          default = python.pkgs.buildPythonApplication attrs;
        }
      );
      
      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/aish";
        };
      });
      
      devShells = forAllSystems (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in {
          default = pkgs.mkShell {
            packages = [
              self.packages.${system}.default
              pkgs.uv
              pkgs.python313
            ];
            
            shellHook = ''
              export UV_PYTHON_PREFERENCE=only-system
            '';
          };
        }
      );
    };
}
