{
  description = "haskell dev env";
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
              packages = with pkgs.haskellPackages; [
                ghc
                haskell-language-server
                cabal-install
              ];

              shellHook = ''
                echo -e "\033[1;32m\nhaskell dev environment loaded"
                echo -e "System:                  ${system}"
                echo -e "ghc:                     $(which ghc 2>/dev/null || echo 'not found')"
                echo -e "haskell-language-server: $(which haskell-language-server-wrapper 2>/dev/null || echo 'not found')"
              '';
            };
          };
        };
    };
}
