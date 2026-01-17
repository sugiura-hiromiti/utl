{
  description = "scheme dev env";
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
              packages = with pkgs; [
                chibi
                akkuPackages.scheme-langserver
              ];

              shellHook = ''
                echo -e "\033[1;32m\nmogok dev environment loaded"
                echo -e "System: ${system}"
                echo -e "chibi-scheme:      $(which chibi-scheme 2>/dev/null || echo 'not found')"
                echo -e "scheme-langserver: $(which scheme-langserver 2>/dev/null || echo 'not found')\033[0m\n"
              '';
            };
          };
        };
    };
}
