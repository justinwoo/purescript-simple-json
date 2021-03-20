let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/39da4240609ee0d8ea533f142ae4c7e25df95980.tar.gz";
    sha256 = "10mp5rjnkl0s6pigbnkdf6pjwm074nf4aq7mwhfwxmz5gs5dpi71";
  };
in

{ pkgs ? import nixpkgs {} }:

  let
    ezPscSrc = pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "e8a1ffafafcdf2e81adba419693eb35f3ee422f8";
      sha256 = "0bk32wckk82f1j5i5gva63f3b3jl8swc941c33bqc3pfg5cgkyyf";
    };
    ezPsc = import ezPscSrc { inherit pkgs; };
  in

    pkgs.mkShell {
      buildInputs = [
        ezPsc.purs-0_14_0
        pkgs.nodePackages_10_x.bower
        pkgs.nodePackages_10_x.pulp
      ];
    }
