{
  description = "ocaml dev env";
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixpkgs-unstable";
    };
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };
    systems = {
      url = "github:nix-systems/default";
    };
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      systems,
      fenix,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import systems;
      perSystem =
        {
          self,
          pkgs,
          lib,
          system,
          ...
        }:
        {
          devShells = {
            default = pkgs.mkShell {
              packages = with pkgs.ocamlPackages; [
                ocaml
                ocaml-lsp
                dune_3
                ocamlformat
                ounit2
                findlib
              ];

              shellHook = ''
                echo -e "\033[1;32m\nocaml dev environment loaded"
                echo -e "System: ${system}"
                echo -e "ocaml:     $(which ocaml 2>/dev/null || echo 'not found')"
                echo -e "ocamlfind: $(which ocamlfind 2>/dev/null || echo 'not found')"
                echo -e "dune:      $(which dune 2>/dev/null || echo 'not found')\033[0m\n"
              '';
            };
          };
        };
    };
}
