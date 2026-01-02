{ inputs, pkgs, lib }:

let
  defaultCompiler = "ghc967";

  cabalProject = pkgs.haskell-nix.cabalProject' (

    { config, pkgs, ... }:

    {
      name = "dmq-node";

      compiler-nix-name = lib.mkDefault defaultCompiler;

      src = lib.cleanSource ../.;

      flake.variants = {
        ghc967 = { }; # Alias for the default variant
        # ghc9122.compiler-nix-name = "ghc9122";
      };

      inputMap = { "https://chap.intersectmbo.org/" = inputs.CHaP; };

      # TODO: enable cross compilation for windows by adding `p.ucrt64`.
      crossPlatforms = p:
        lib.optionals (pkgs.stdenv.hostPlatform.isLinux && config.compiler-nix-name == defaultCompiler)
          [ p.musl64 ];

      modules = [
        {
          packages.dmq-node.components.tests.dmq-cddl.build-tools = [ pkgs.cddl pkgs.cbor-diag pkgs.cddlc ];
          packages.dmq-node.components.tests.dmq-cddl.preCheck = "export HOME=`pwd`";
        }
        {
          packages = lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
            # ruby fails to build with musl, hence we disable cddl tests
            dmq-node.components.tests.dmq-cddl.build-tools = lib.mkForce [ ];
            dmq-node.components.tests.dmq-cddl.doCheck = lib.mkForce false;
            dmq-node.ghcOptions = with pkgs; [
              "-L${lib.getLib static-gmp}/lib"
              "-L${lib.getLib static-libsodium-vrf}/lib"
              "-L${lib.getLib static-secp256k1}/lib"
              "-L${lib.getLib static-libblst}/lib"
            ];
          };
        }
      ];
    }
  );

in

cabalProject
