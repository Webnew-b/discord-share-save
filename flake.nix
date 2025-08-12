{
  description = "Haskell development environment with matching GHC and HLS versions";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # 使用 GHC 9.2.8 来匹配 LTS-20.26
        ghcVersion = "928";
        haskellPackages = pkgs.haskell.packages."ghc${ghcVersion}";
        
        # 确保 HLS 版本与 GHC 兼容
        hls = haskellPackages.haskell-language-server;
        
        # 开发工具
        devTools = with pkgs; [
          # Haskell 工具链
          haskellPackages.ghc
          haskellPackages.cabal-install
          haskellPackages.stack
          hls
          
          # 开发工具
          haskellPackages.ghcid
          haskellPackages.hlint
          haskellPackages.ormolu  # 代码格式化
          haskellPackages.hpack   # 如果你使用 package.yaml
          
          # 系统工具和库
          zlib
          pkg-config
          
          # Discord bot 开发可能需要的系统库
          openssl
          libffi
          gmp
          
          # 网络和加密相关
          curl
          cacert
        ];
        
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = devTools;
          
          # 设置环境变量
          shellHook = ''
            echo "🤖 Discord Haskell Bot Development Environment"
            echo "📦 GHC version: $(ghc --version)"
            echo "🔧 HLS version: $(haskell-language-server-wrapper --version | head -1)"
            echo "📚 Stack LTS: 20.26 (GHC 9.2.8)"
            echo "🎮 Extra deps: discord-haskell-1.16.1, emojis-0.1.4.1"
            echo ""
            echo "💡 Tips:"
            echo "  - Use 'stack build' to build with extra-deps"
            echo "  - Run 'stack ghci' for REPL with discord-haskell loaded"
            echo "  - HLS should work correctly with Neovim now"
            echo "  - Use 'ghcid --command=\"stack ghci\"' for continuous compilation"
            echo ""
            echo "🔧 Available tools:"
            echo "  - ghc, cabal, stack, ghcid, hlint, ormolu"
            echo "  - haskell-language-server for LSP"
            
            # 确保 stack 使用系统 GHC 和正确的 zlib
            export STACK_SYSTEM_GHC=1
            export STACK_ROOT="$(pwd)/.stack"
            
            # 设置 zlib 标志以匹配你的 stack.yaml
            export STACK_YAML="$(pwd)/stack.yaml"
          '';
          
          # 添加库路径（Discord bot 和网络库需要）
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.zlib
            pkgs.openssl
            pkgs.libffi
            pkgs.gmp
          ];
        };
        
        # 为 Neovim 用户提供的额外配置
        packages.neovim-config = pkgs.writeText "haskell-lsp-config.lua" ''
          -- Haskell LSP 配置
          require'lspconfig'.hls.setup{
            filetypes = { "haskell", "lhaskell", "cabal" },
            cmd = { "haskell-language-server-wrapper", "--lsp" },
            settings = {
              haskell = {
                formattingProvider = "ormolu",
                cabalFormattingProvider = "cabalfmt",
              }
            },
            on_attach = function(client, bufnr)
              -- 启用格式化
              if client.server_capabilities.documentFormattingProvider then
                vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>f', '<cmd>lua vim.lsp.buf.format()<CR>', {noremap=true, silent=true})
              end
            end,
          }
        '';
        
        # 为你的项目创建包含所需依赖的 Haskell 环境
        packages.haskell-env = haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
          # LTS-20.26 核心包
          base
          text
          aeson
          bytestring
          containers
          time
          mtl
          transformers
          stm
          async
          
          # Discord 和相关包
          # discord-haskell 和 emojis 需要通过 stack 安装（extra-deps）
          # 但这里添加它们可能的依赖
          websockets
          http-client
          http-client-tls
          network
          network-uri
          base64-bytestring
          
          # 常用的实用包
          lens
          vector
          unordered-containers
          hashable
          scientific
          uuid
          random
          filepath
          directory
          process
          
          # 解析和序列化
          parser-combinators
          megaparsec
          attoparsec
          yaml
          
          # 并发和异步
          concurrent-supply
          
          # 日志和调试
          monad-logger
          fast-logger
          
          # HTTP 和网络
          req
          servant
          servant-client
          warp
          
          # 数据库（如果需要）
          # postgresql-simple
          # sqlite-simple
          
          # 测试
          hspec
          QuickCheck
          tasty
          tasty-hspec
          tasty-quickcheck
        ]);
      }
    );
}
