let nixpkgs = <nixpkgs>;
    config = {};
in with import nixpkgs { inherit config; }; let
  shellWrapper = writeScript "shell-wrapper" ''
    #! ${stdenv.shell}
    exec -a shell ${fish}/bin/fish "$@"
  '';

  gnumakeWithGuile = gnumake.override {
    guileSupport = true;
    guile = guile_1_8;
  };
  libs = [
    cairo
    fontconfig
    glib
    glibc.static
    gmp
    gtk3
    gsettings-desktop-schemas
    libedit
    libGL
    libGLU
    libjpeg
    libpng
    mpfr
    openssl
    pango
    poppler
    readline
    sqlite
    racket
  ];
  libPath = stdenv.lib.makeLibraryPath libs;
in stdenv.mkDerivation rec {
  name = "nix-shell";
  buildInputs = [
    glibcLocales bashInteractive man
    nix cacert curl utillinux coreutils
    git jq tmux
    findutils patchelf pkg-config
    gnumakeWithGuile

    racket etcd
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="${libPath}"
    export LANG=en_US.UTF-8
    export SHELL="${shellWrapper}"
    export NIX_PATH=nixpkgs=${nixpkgs}
    export CFLAGS="-I${racket}/include/racket"
    export LDFLAGS="-L${lib.concatStringsSep " -L" (map (v: "${builtins.toString v}/lib")
      libs)}"
  '';
}
