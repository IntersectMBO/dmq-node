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

  ciJobs =
    flake.hydraJobs
    //
    {
      # Keep haskell.nix component builds (`dmq-node:lib:dmq-node`,
      # `dmq-node:test:*`, …) alongside our own packages.
      packages = flake.hydraJobs.packages // packages;
      inherit devShells;
      inherit format;
    };

  # Sub-groups in `flake.hydraJobs` are compiler variants (e.g. `ghc9122`) and
  # cross-compilation targets (e.g. `x86_64-unknown-linux-musl`).  They are
  # distinguished from native job categories (`packages`, `checks`, …) by
  # having nested attrsets as values rather than derivations.
  subGroups = lib.filterAttrs
    (_: v:
      lib.isAttrs v
      && !lib.isDerivation v
      && lib.any (x: lib.isAttrs x && !lib.isDerivation x)
        (lib.attrValues v))
    flake.hydraJobs;

  # Native-only jobs: `ciJobs` without the sub-groups, so that the top-level
  # `all` is symmetric across systems.
  nativeJobs = removeAttrs ciJobs (builtins.attrNames subGroups);

  defaultHydraJobs =
    ciJobs
    # For each sub-group expose an `all` aggregate, e.g.:
    #   nix build .\#hydraJobs.x86_64-linux.ghc9122-all
    #   nix build .\#hydraJobs.x86_64-linux.x86_64-unknown-linux-musl-all
    // lib.mapAttrs
      (name: jobs: jobs // {
        all = pkgs.releaseTools.aggregate {
          name = "${name}-all";
          meta.description = "All jobs for ${name} (no tests)";
          constituents = utils.collectDerivationsWithoutChecks jobs;
        };
      })
      subGroups
    //
    {
      # An `all` aggregate covering every native job on this system,
      # excluding tests, e.g.:
      #   nix build .\#hydraJobs.x86_64-linux.all
      all = pkgs.releaseTools.aggregate {
        name = "all";
        meta.description = "All native jobs for ${system} (no tests)";
        constituents = utils.collectDerivationsWithoutChecks nativeJobs;
      };
      required = utils.makeHydraRequiredJob ciJobs;
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
