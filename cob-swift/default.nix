{ pkgs ? import <nixpkgs> {} }:

let
  inherit (pkgs);

  # Ensure static libraries are available
  gmp6 = pkgs.gmp6.override {
    withStatic = true;
  };

  libffi = pkgs.libffi.overrideAttrs (old: {
    dontDisableStatic = true;
  });

  zlibStatic = pkgs.zlib.overrideAttrs (old: {
    dontDisableStatic = true;
  });

  # Haskell package set
  haskellPackages = pkgs.haskellPackages;

  # Build cob-swift with cabal2nix
  cob-swift = haskellPackages.callCabal2nix "cob-swift" ./. {};

  # Apply cabal overrides
  cob-swift-static = pkgs.haskell.lib.overrideCabal cob-swift (old: {
    configureFlags = (old.configureFlags or [ ]) ++ [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-Wl,-search_paths_first"
      "--extra-lib-dirs=${gmp6}/lib"
      "--extra-lib-dirs=${libffi}/lib"
      "--extra-lib-dirs=${zlibStatic}/lib"
    ];

    enableSharedExecutables = false;
    enableSharedLibraries = false;
    enableStatic = true;
  });

in
  cob-swift-static

