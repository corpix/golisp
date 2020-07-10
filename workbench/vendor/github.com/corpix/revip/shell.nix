let nixpkgs = <nixpkgs>;
in with import nixpkgs {}; let
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
    go golangci-lint
    gnumakeWithGuile
  ];
  shellHook = ''
    export LANG=en_US.UTF-8
    export SHELL="${shellWrapper}"
    export NIX_PATH=nixpkgs=${nixpkgs}

    unset GOPATH

    local p
    for p in $buildInputs
    do
      export XDG_DATA_DIRS="$XDG_DATA_DIRS:$p/share"
    done
  '';
}
