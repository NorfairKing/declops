{ configurationPath
}:
let
  nixpkgs = <nixpkgs>;
  pkgs = import nixpkgs { };
  nPkgsv = import (nixpkgs + "/nixos/default.nix");
  makeDiskImage = import (nixpkgs + "/nixos/lib/make-disk-image.nix");
  config = (nPkgsv {
    configuration = import ./configurationPath;
    inherit configuration;
  }).config;
in
makeDiskImage {
  inherit pkgs;
  lib = pkgs.lib;
  inherit config;
  format = "vdi";
}
