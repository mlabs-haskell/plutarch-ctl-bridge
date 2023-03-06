{
  description = "plutarch-ctl-bridge";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    plutarch = {
      follows = "liqwid-libs/liqwid-nix/plutarch";
    };
    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix";
      inputs.plutarch.follows = "plutarch";
    };
    liqwid-libs = {
      url = "github:Liqwid-Labs/liqwid-libs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.liqwid-nix.ci
        inputs.liqwid-nix.onchain
        inputs.liqwid-nix.run
        ./flake-module.nix
      ];
      systems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" "aarch64-linux" ];
      flake.herculesCI.ciSystems = [ "x86_64-linux" ];
      flake.flakeModule = ./flake-module.nix;
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        onchain.default = {
          src = ./.;
          ghc.version = "ghc925";
          fourmolu.package = pkgs.haskell.packages.ghc943.fourmolu_0_10_1_0;
          applyRefact.package = pkgs.haskell.packages.ghc924.apply-refact_0_11_0_0;
          hlint = { };
          cabalFmt = { };
          hasktags = { };
          shell = { };
          hoogleImage = { };
          enableBuildChecks = true;
          extraHackageDeps = [
            "${inputs.liqwid-libs}/plutarch-quickcheck"
            "${inputs.liqwid-libs}/plutarch-context-builder"
            "${inputs.liqwid-libs}/liqwid-plutarch-extra"
            "${inputs.liqwid-libs}/liqwid-script-export"
            "${inputs.liqwid-libs}/plutarch-unit"
            "${inputs.liqwid-libs.inputs.ply}/ply-core"
            "${inputs.liqwid-libs.inputs.ply}/ply-plutarch"
          ];
        };
        ci.required = [ "all_onchain" ];

        ctlBridge.example = {
          template = ./ctl-types-template;
          package = self'.packages."plutarch-ctl-bridge:exe:example";
          binName = "example";
          outName = "out";
        };
      };
    };
}
