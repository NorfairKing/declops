let
  nixpkgs = <nixpkgs>;
  nPkgsv = import (nixpkgs + "/nixos/default.nix");
  releaseF = import (nixpkgs + "/nixos/release.nix");
  minimal-iso = (releaseF { supportedSystems = [ "x86_64-linux" ]; }).iso_minimal.x86_64-linux;
in
minimal-iso
