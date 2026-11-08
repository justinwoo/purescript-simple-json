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
    (ezPsc.purs-0_15_0.overrideAttrs (_: {
      version = "v0.15.4";
      src = pkgs.fetchurl (
        if pkgs.stdenv.isDarwin then {
          url = "https://github.com/purescript/purescript/releases/download/v0.15.4/macos.tar.gz";
          sha256 = "cd08d63f62c7fcb553611352f07fd003bf2baf3239aaad1845e45e9498617a66";
        } else {
          url = "https://github.com/purescript/purescript/releases/download/v0.15.4/linux64.tar.gz";
          sha256 = "3e2a59dbac49564b7ceedb5b102eecf2b0b24765dbfcc1eed5ac4ea396c6e68e";
        }
      );
    }))
  ];
}
