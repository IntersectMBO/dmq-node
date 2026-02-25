{ inputs, pkgs, lib, utils, ghc }:

let
  dmq-node = pkgs.dmq-node;
  tool = dmq-node.projectVariants.${ghc}.tool;
  tools = {
    cabal = tool "cabal" "latest";
    cabal-gild = tool "cabal-gild" "latest";
    haskell-language-server = tool "haskell-language-server" "latest";
    hlint = tool "hlint" "latest";
  };

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

  shell = dmq-node.shellFor {
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
