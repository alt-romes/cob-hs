{ pkgs ? import <nixpkgs> {}, ... }:
pkgs.mkShell {
  buildInputs = with pkgs; [
    # I think this is for building cob-xlsx
    zlib pkg-config expat bzip2
  ];  
}
