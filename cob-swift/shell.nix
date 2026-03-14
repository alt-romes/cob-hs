# NOTE: RUN WITH nix-shell --pure!!
# THEN BUILD WITH cabal build cob-swift $EXTRA_GHC_OPTS
{ pkgs ? import <nixpkgs> {} }:
let
  libffiStaticOnly = pkgs.libffi.overrideAttrs (oldAttrs: rec {
    configureFlags = (oldAttrs.configureFlags or []) ++ [ "--enable-static" "--disable-shared" ];
  });

  gmpStaticOnly = pkgs.gmp.overrideAttrs (oldAttrs: rec {
    configureFlags = (oldAttrs.configureFlags or []) ++ [ "--enable-static" "--disable-shared" ];
  });

  # MacOS's libSystem includes its own iconv.
  # We have to filter out -liconv from invocations of the linker by GHC because
  # it's weirdly linking against some iconv found in a nix store path...
  # let's just see it work. eventually do something about this...
  filter_iconv_wrapper =
  let sedCmd = "sed -E 's/-liconv//g; s/ -L\\/nix\\/store\\/.*libiconv.*\\/lib //g'"; in
  pkgs.writeShellScriptBin "static-cc-wrapper" ''
    # Determine the real C compiler command used by GHC.
    # We extract the path directly to avoid issues with spaces or special characters.
    real_clang="$(ghc --info | grep "C compiler command" | sed -E 's/.*"([^"]+)".*/\1/')"

    declare -a processed_args

    for arg in "$(echo $@ | ${sedCmd})"; do
      if [[ "$arg" == @* ]]; then
        response_file="${"$"}{arg#@}"
        if [[ -f "$response_file" ]]; then
          temp_response_file=$(mktemp)
          cat "$response_file" | ${sedCmd} > "$temp_response_file"
          processed_args+=("@$temp_response_file")
        else
          processed_args+=("$arg")
        fi
      else
        processed_args+=("$arg")
      fi
    done
    echo "PROCESSED ARGS:" ${"$"}{processed_args[@]}
    exec "$real_clang" ${"$"}{processed_args[@]}
  '';

in
pkgs.mkShell {
  buildInputs = [
    pkgs.git
    pkgs.openssh
    pkgs.haskell.compiler.ghc910
    pkgs.haskell.packages.ghc910.cabal-install
  ];

  STATIC_HASKELL_CABAL_OPTS
    = "--extra-lib-dirs=${pkgs.zlib.static}/lib\
       --extra-lib-dirs=${libffiStaticOnly}/lib\
       --extra-lib-dirs=${gmpStaticOnly}/lib\
       --ghc-options=-pgml=${filter_iconv_wrapper}/bin/static-cc-wrapper\
      ";
}
