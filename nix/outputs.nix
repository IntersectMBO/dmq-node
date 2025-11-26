{ inputs, system }:

let
  inherit (pkgs) lib;

  pkgs = import ./pkgs.nix { inherit inputs system; };

  utils = import ./utils.nix { inherit pkgs lib; };

  project = import ./project.nix { inherit inputs pkgs lib; };

  mkShell = ghc: import ./shell.nix { inherit inputs pkgs lib project utils ghc; };

  packages = rec {
    # TODO: `nix build .\#dmq-node` will have the git revision set in the binary,
    # `nib build .\#hydraJobs.x86_64-linux.packages.dmq-node:exe:dmq-node` won't
    dmq-node =
      # setGitRev broken?
      # pkgs.setGitRev
      # (inputs.self.rev or inputs.self.dirtyShortRev)
      project.hsPkgs.dmq-node.components.exes.dmq-node;
    dmq-node-static =
      # pkgs.setGitRev
      # (inputs.self.rev or inputs.self.dirtyShortRev)
      project.projectCross.musl64.hsPkgs.dmq-node.components.exes.dmq-node;
    default = dmq-node;
  };

  app = {
    default = packages.dmq-node;
  };

  devShells = rec {
    default = ghc967;
    ghc967 = mkShell "ghc967";

    # ghc9122 = mkShell "ghc9122";
  };

  flake = project.flake { };

  defaultHydraJobs = {
    ghc967 = flake.hydraJobs.ghc967;
    inherit packages;
    inherit devShells;
    required = utils.makeHydraRequiredJob hydraJobs;
    preCommitCheck = devShells.default.preCommitCheck;
  };

  hydraJobsPerSystem = {
    "x86_64-linux" = defaultHydraJobs;
    "x86_64-darwin" = defaultHydraJobs;
    "aarch64-linux" = { };
    "aarch64-darwin" = defaultHydraJobs;
  };

  hydraJobs = utils.flattenDerivationTree "-" hydraJobsPerSystem.${system};
in

{
  inherit packages;
  inherit app;
  inherit devShells;
  inherit hydraJobs;
  __internal = { inherit pkgs project; };
}
