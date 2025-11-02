{
  description = "Discord Share Save — Haskell Discord bot with Nix flake dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

      in {
        # Development shell for Stack / GHC
        devShells.default = pkgs.mkShell {
          name = "discord-share-save";

          # 🧰 Haskell + system libraries
          buildInputs = with pkgs; [
            haskell.compiler.ghc96
            haskellPackages.stack
            zlib
            openssl
            pkg-config
            git
          ];
        DISCORD_SECRET = builtins.getEnv "DISCORD_SECRET";
        
        shellHook = ''
            if [ -f .env ]; then
              echo "📦 Loading environment from .env ..."
              export $(grep -v '^#' .env | xargs)
            fi
            echo "🚀 Entered Nix dev shell (GHC 9.6.7)"
          '';
        };
      });
}

