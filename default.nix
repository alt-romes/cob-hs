{ pkgs ? import <nixpkgs> {}, ... }:
let
  streamly-repo = (pkgs.fetchFromGitHub {
    repo = "streamly";
    owner = "composewell";
    rev = "master";
    sha256 = "sha256-aMQ9uAeKSe1m/G0cO14xgKx5njiTptQ/XtWxbwnB3k0=";

  });
  hspkgs = pkgs.haskell.packages.ghc910.override {
    overrides = self: super: rec {

      # Updated versions
      streamly-core = pkgs.haskell.lib.doJailbreak super.streamly-core;
      streamly = pkgs.haskell.lib.doJailbreak super.streamly;
      uuid = pkgs.haskell.lib.doJailbreak super.uuid;
      zip = pkgs.haskell.lib.doJailbreak super.zip;
      generics-sop =
        # generics-sop-0.5.1.4-r2
        pkgs.haskell.lib.compose.overrideCabal
        (drv: {
          revision = "2"; # this is how to get revision 2
          editedCabalFile = "sha256-pihkoKCW+ETblGo4w7mYHyDMUMs274u7FItpWXauTHU=";
        })
        (self.callHackageDirect {
          pkg = "generics-sop";
          ver = "0.5.1.4";
          sha256 = "sha256-dQ6Y2PesKeu21zmzsSi+ylYp3qTXYtmIlYoFT2dCICo=";
        } {});

      # hs-mcp = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "hs-mcp" (pkgs.fetchFromGitHub {
      #   repo = "hs-mcp";
      #   owner = "alt-romes";
      #   rev = "master";
      #   sha256 = lib.fakeHash;
      # }) {});
      hs-mcp = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "hs-mcp" ../hs-mcp {});

      mermaid = pkgs.haskell.lib.doJailbreak (self.callCabal2nix "mermaid" (pkgs.fetchFromGitHub {
        repo = "mermaid-hs";
        owner = "alt-romes";
        rev = "master";
        sha256 = "sha256-aMQ9uAeKSe1m/G0cO14xgKx5njiTptQ/XtWxbwnB3k0=";
      }) {});

      cob = self.callCabal2nix "cob" ./. {};
      cob-mcp = self.callCabal2nix "cob-mcp" ./cob-mcp {};
    };
  };
in
{
  cob = hspkgs.cob;
  cob-mcp = hspkgs.cob-mcp;
}
