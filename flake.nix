{
  description = "Simple grep-like tool implemented in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
      in {
        packages.default = haskellPackages.callCabal2nix "simple-grep" ./. {};

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell development tools
            haskellPackages.cabal-install
            haskellPackages.ghc
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ormolu
          ];

          # Make sure our Haskell dependencies are available
          inputsFrom = [ self.packages.${system}.default ];

          # Set up environment variables for development
          shellHook = ''
            echo "Haskell development environment loaded!"
            echo "Using GHC version: $(ghc --version)"
            echo "Using Cabal version: $(cabal --version | head -n 1)"
            echo ""
            echo "Available commands:"
            echo "  cabal build         - Build the project"
            echo "  cabal run simple-grep -- [ARGS] - Run the grep tool"
            echo "  cabal test          - Run tests"
            echo "  cabal repl          - Start a REPL"
            echo "  haskell-language-server - Start language server"
            echo "  hlint .             - Run linter on the source code"
          '';
        };
      }
    );
}
