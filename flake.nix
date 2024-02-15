{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nci = {
      url = "github:yusdacra/nix-cargo-integration";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs @ { flake-parts, flake-utils, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = flake-utils.lib.defaultSystems;
      imports = [ inputs.nci.flakeModule ];
      perSystem = { config, pkgs, ... }:
        let
          outputs = config.nci.outputs.rq;
          toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
        in
        {
          nci = {
            toolchainConfig = ./rust-toolchain.toml;
            projects.rq-project = {
              path = ./.;
            };
            crates.rq = { };
          };

          packages = rec {
            rq = outputs.packages.release;
            default = rq;
          };

          devShells.default = pkgs.mkShell { buildInputs = [ toolchain pkgs.rust-analyzer ]; };
        };
    };
}
