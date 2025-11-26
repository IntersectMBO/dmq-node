{ inputs, pkgs, lib, project, utils, ghc }:

let

  allTools = {
    "ghc967".cabal = project.projectVariants.ghc967.tool "cabal" "latest";
    "ghc967".cabal-gild = project.projectVariants.ghc967.tool "cabal-gild" "latest";
    "ghc967".haskell-language-server = project.projectVariants.ghc967.tool "haskell-language-server" "latest";
    "ghc967".hlint = project.projectVariants.ghc967.tool "hlint" "latest";

    "ghc9122".cabal = project.projectVariants.ghc9122.tool "cabal" "latest";
    "ghc9122".cabal-gild = project.projectVariants.ghc9122.tool "cabal-gild" "latest";
    "ghc9122".haskell-language-server = project.projectVariants.ghc9122.tool "haskell-language-server" "latest";
    "ghc9122".hlint = project.projectVariants.ghc9122.tool "hlint" "latest";
  };

  tools = allTools.${ghc};

  preCommitCheck = inputs.pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSource ../.;

    hooks = {
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
        args = [ "--hint" ".stylish-haskell.yaml" ];
      };
      nixpkgs-fmt = {
        enable = true;
      };
      cabal-gild = {
        enable = true;
        entry = "${tools.cabal-gild}/bin/cabal-gild";
        files = ".*\\.cabal$";
        args = [ "--io" ];
        stages = [ "pre-commit" ];
        pass_filenames = true;
      };
      cabal-check = {
        enable = true;
        entry =
          let
            script = pkgs.writeShellScript "cabal-check" ''
              pushd $(dirname $1)
              ${tools.cabal}/bin/cabal check || exit 1
              popd
            '';
          in
          "${script}";
        files = ".*\\.cabal$";
        stages = [ "pre-commit" ];
        pass_filenames = true;
      };
    };
  };

  linuxPkgs = lib.optionals pkgs.hostPlatform.isLinux [
  ];

  darwinPkgs = lib.optionals pkgs.hostPlatform.isDarwin [
  ];

  commonPkgs = [
    tools.haskell-language-server
    tools.haskell-language-server.package.components.exes.haskell-language-server-wrapper
    tools.cabal
    tools.cabal-gild
    tools.hlint

    pkgs.shellcheck
    pkgs.github-cli
    pkgs.act
    pkgs.bzip2
    pkgs.gawk
    pkgs.zlib
    pkgs.cacert
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.which
  ];

  shell = project.shellFor {
    name = "dmq-node-shell-${ghc}";

    buildInputs = lib.concatLists [
      commonPkgs
      darwinPkgs
      linuxPkgs
    ];

    withHoogle = true;

    shellHook = ''
      ${preCommitCheck.shellHook}
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    '';
  };

in

shell // {
  inherit preCommitCheck;
}
