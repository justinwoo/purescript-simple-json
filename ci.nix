let
  nixpkgs = builtins.fetchTarball {
    # Commit to release-20.03 on 23 June 2020
    url = "https://github.com/nixos/nixpkgs/archive/39da4240609ee0d8ea533f142ae4c7e25df95980.tar.gz";
    sha256 = "10mp5rjnkl0s6pigbnkdf6pjwm074nf4aq7mwhfwxmz5gs5dpi71";
  };
in

{ pkgs ? import nixpkgs {} }:

let
  # Latest commit to the master branch as of 25 June 2020
  ezPscSrc = pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "2432d492b1f0cc3aa7bf8b4bdb2c7ee0219e1f4e";
    sha256 = "0gdg7b4yvs9rg2z6xv705miq5c9vbhimq21nfmhk9j48b3m3xkhf";
  };
  ezPsc = import ezPscSrc { inherit pkgs; };
in

pkgs.mkShell {
  buildInputs = [
    ezPsc.purs
    pkgs.nodePackages_10_x.bower
    pkgs.nodePackages_10_x.pulp
  ];
}
