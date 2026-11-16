{
  description = "DMQ Node";

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
    accept-flake-config = true;
  };

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix/77645ec92f1d019695f8bf291852a5c617138e3f";
      # url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem
    (system:
      import ./nix/outputs.nix { inherit inputs system; }
    ) // {
    overlays.default = final: _prev: {
      inherit (inputs.self.packages.${final.stdenv.hostPlatform.system}) dmq-node;
    };

    # Defaults `package` directly rather than adding the overlay: NixOS rejects
    # `nixpkgs.overlays` when `nixpkgs.pkgs` is set from outside.
    nixosModules = rec {
      dmq-node = { lib, pkgs, ... }: {
        imports = [ ./nix/nixos/dmq-node-service.nix ];
        services.dmq-node.package = lib.mkDefault
          inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.dmq-node;
      };
      default = dmq-node;
    };
  };
}
