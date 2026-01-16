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

  linuxPkgs = lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  ];

  darwinPkgs = lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
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
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    '';
  };

in

shell
