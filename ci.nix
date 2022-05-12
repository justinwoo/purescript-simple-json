let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/39da4240609ee0d8ea533f142ae4c7e25df95980.tar.gz";
    sha256 = "10mp5rjnkl0s6pigbnkdf6pjwm074nf4aq7mwhfwxmz5gs5dpi71";
  };
in

{ pkgs ? import nixpkgs { } }:

let
  ezPscSrc = pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "0ad5775c1e80cdd952527db2da969982e39ff592";
    sha256 = "0x53ads5v8zqsk4r1mfpzf5913byifdpv5shnvxpgw634ifyj1kg";
  };
  ezPsc = import ezPscSrc { inherit pkgs; };
in

pkgs.mkShell {
  buildInputs = [
    ezPsc.purs-0_15_0
    ezPsc.pulp
    pkgs.nodePackages_10_x.bower
  ];
}
