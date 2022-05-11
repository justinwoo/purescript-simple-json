let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz";
    sha256 = "162dywda2dvfj1248afxc45kcrg83appjd0nmdb541hl7rnncf02";
  };
in

{ pkgs ? import nixpkgs {} }:

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
        ezPsc.psc-package
        ezPsc.spago
        pkgs.nodejs
      ];
    }
