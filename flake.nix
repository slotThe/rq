{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, rust-overlay, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ rust-overlay.overlays.default ];
        pkgs = import nixpkgs { inherit system overlays; };
        toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
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
              description =
                "a tiny functional language with which you can manipulate JSON";
            };
            cargoLock.lockFile = ./Cargo.lock;
          };

          default = self.packages."${system}".rq;
        };

        devShells.default =
          mkShell { buildInputs = [ toolchain rust-analyzer ]; };
      });
}
