{
  description = "development flake for impli in the web";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    ghcWasmMeta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs =
    {
      self,
      nixpkgs,
      ghcWasmMeta,
      ...
    }:
    let
      system = "aarch64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          emscripten
          ghcWasmMeta.packages.${system}.default
          wizer
          binaryen
          deno
        ];
      };
    };
}
