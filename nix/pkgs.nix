{ inputs, system }:

import inputs.nixpkgs {
  inherit system;
  config = inputs.haskell-nix.config;
  overlays = [
    # haskellNix.overlay can be configured by later overlays, so need to come before them.
    inputs.haskell-nix.overlay
    # dmq-node depends cardano-crypto-class, so we need crypto overlays
    inputs.iohk-nix.overlays.crypto
    inputs.iohk-nix.overlays.cardano-lib
    inputs.iohk-nix.overlays.haskell-nix-crypto
    inputs.iohk-nix.overlays.haskell-nix-extra
    (final: _prev: {
      static-libsodium-vrf = final.libsodium-vrf.overrideDerivation (old: {
        configureFlags = old.configureFlags ++ [ "--disable-shared" ];
      });
      static-secp256k1 = final.secp256k1.overrideDerivation (old: {
        configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ];
      });
      static-gmp = (final.gmp.override { withStatic = true; }).overrideDerivation (old: {
        configureFlags = old.configureFlags ++ [ "--enable-static" "--disable-shared" ];
      });
      static-libblst = (final.libblst.override { enableShared = false; }).overrideDerivation (_old: {
        postFixup = "";
      });
    })
  ];
}
