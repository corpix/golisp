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
in stdenv.mkDerivation rec {
  name = "nix-shell";
  buildInputs = [
    glibcLocales bashInteractive man
    nix cacert curl utillinux coreutils
    git jq tmux
    findutils patchelf
    gnumakeWithGuile

    racket etcd
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${pkgs.openssl.out}/lib
    export LANG=en_US.UTF-8
    export SHELL="${shellWrapper}"
    export NIX_PATH=nixpkgs=${nixpkgs}
  '';
}

