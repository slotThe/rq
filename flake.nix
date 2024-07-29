{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
        toolchain = (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml).override {
          extensions = [ "rustc-codegen-cranelift-preview" ];
        };
        platform = pkgs.makeRustPlatform {
          cargo = toolchain;
          rustc = toolchain;
        };
        cargoMeta =
          (builtins.fromTOML (builtins.readFile ./Cargo.toml)).package;
      in with pkgs; {
        packages = {
          rq = platform.buildRustPackage {
            pname = "rq";
            inherit (cargoMeta) version;
            src = ./.;
            meta = {
              license = lib.licenses.gpl3;
              description = "a tiny functional language to manipulate JSON";
            };
            cargoLock = {
              outputHashes."chumsky-1.0.0-alpha.6" = "sha256-zQYygpL3m7EoqShn2pag8NAXOgv4qr4L3l8KotwbJ7w=";
              lockFile = ./Cargo.lock;
            };
          };

          default = self.packages."${system}".rq;
        };

        devShells.default =
          mkShell { buildInputs = [ toolchain rust-analyzer ]; };
      });
}
