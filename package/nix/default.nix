{ pkgs   ? import <nixpkgs> {},
  stdenv ? pkgs.stdenv,
  gcc ? pkgs.gcc,
  gmp ? pkgs.gmp,
  git ? pkgs.git,
  gnumake ? pkgs.gnumake,
  binutils ? pkgs.binutils-unwrapped,
  bash ? pkgs.bash,
  mlton ? pkgs.mlton
}:

stdenv.mkDerivation rec {
  name = "mlton";

  # The root "mlton" directory.
  src = ../../.;

  buildInputs = [ gcc gmp git gnumake binutils bash mlton ];

  doCheck = true;

  buildPhase = ''
    find . -type f | grep -v -e '\.tgz''$' | xargs sed -i "s@/usr/bin/env bash@$(type -p bash)@"
    make
  '';

  installPhase = ''
    make install PREFIX=$out
  '';

}
