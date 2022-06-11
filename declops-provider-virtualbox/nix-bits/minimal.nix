let
  nixpkgs = <nixpkgs>;
  pkgs = import nixpkgs { };
  nPkgsv = import (nixpkgs + "/nixos/default.nix");
  makeDiskImage = import (nixpkgs + "/nixos/lib/make-disk-image.nix");
  configForConfiguration = configuration:
    (nPkgsv {
      inherit configuration;
    }).config;
  configuration = { ... }: {
    imports = [
      (nixpkgs + "/nixos/modules/virtualisation/virtualbox-image.nix")
    ];
  }; # Empty configuration
in
makeDiskImage {
  inherit pkgs;
  lib = pkgs.lib;
  config = configForConfiguration configuration;
  format = "vdi";
}
