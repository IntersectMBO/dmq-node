{ inputs, system }:

let
  inherit (pkgs) lib;

  # pkgs contains `dmq-node` included as an overlay of nixpkgs
  pkgs = import ./pkgs.nix { inherit inputs system; };

  utils = import ./utils.nix { inherit pkgs lib; };

  mkShell = ghc: import ./shell.nix { inherit inputs pkgs lib utils ghc; };

  buildSystem = pkgs.stdenv.buildPlatform.system;

  # Fully static, natively-built variant for each system we support it on.
  # Both are libc swaps for the build platform's own arch, not a cross-arch
  # build (see `crossPlatforms` in nix/dmq-node.nix).
  staticCrossTarget = {
    "x86_64-linux" = "musl64";
    "aarch64-linux" = "aarch64-multiplatform-musl";
  };

  packages = rec {
    # TODO: `nix build .\#dmq-node` will have the git revision set in the binary,
    # `nib build .\#hydraJobs.x86_64-linux.packages.dmq-node:exe:dmq-node` won't
    dmq-node =
      # setGitRev broken?
      # pkgs.setGitRev
      # (inputs.self.rev or inputs.self.dirtyShortRev)
      pkgs.dmq-node.hsPkgs.dmq-node.components.exes.dmq-node;
    default = dmq-node;
  } // lib.optionalAttrs (builtins.hasAttr buildSystem staticCrossTarget) {
    dmq-node-static =
      # pkgs.setGitRev
      # (inputs.self.rev or inputs.self.dirtyShortRev)
      pkgs.dmq-node.projectCross.${staticCrossTarget.${buildSystem}}.hsPkgs.dmq-node.components.exes.dmq-node;
    docker-dmq = pkgs.dockerTools.buildImage {
      name = "docker-dmq-node";
      tag = "latest";
      created = "now";
      copyToRoot = pkgs.buildEnv {
        name = "dmq-env";
        paths = [
          pkgs.busybox
          pkgs.dockerTools.caCertificates
        ];
      };
      config = {
        Entrypoint = [ "${packages.dmq-node-static}/bin/dmq-node" ];
      };
    };
  };

  app = {
    default = packages.dmq-node;
  };

  devShells = rec {
    default = mkShell pkgs.dmq-node.args.compiler-nix-name;
    # ghc9122 = mkShell "ghc9122";
  };

  flake = pkgs.dmq-node.flake { };
  format = pkgs.callPackage ./formatting.nix pkgs;

  defaultHydraJobs =
    flake.hydraJobs
    //
    {
      inherit packages;
      inherit devShells;
      inherit format;
      required = utils.makeHydraRequiredJob hydraJobs;
    };

  hydraJobs =
    utils.flattenDerivationTree "-"
      {
        "x86_64-linux" = defaultHydraJobs;
        "x86_64-darwin" = { };
        "aarch64-linux" = defaultHydraJobs;
        "aarch64-darwin" = defaultHydraJobs;
      }.${system};
in

{
  inherit packages;
  inherit devShells;
  inherit hydraJobs;
  legacyPackages = {
    format =
      format
      // {
        all = pkgs.releaseTools.aggregate {
          name = "dmq-node-format";
          meta.description = "Run all formatters";
          constituents = lib.collect lib.isDerivation format;
        };
      };
  };
  __internal = { inherit pkgs; };
}
