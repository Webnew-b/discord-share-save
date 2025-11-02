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

          # 环境变量（必要时可以在这里传 TOKEN、LIB_PATH）
          shellHook = ''
            echo "🚀 Welcome to Discord-Share-Save dev environment!"
            echo "💡 Use 'stack build' or 'stack run' to start."
          '';
        };
      });
}

