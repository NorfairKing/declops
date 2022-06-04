{ sources ? import ../../nix/sources.nix
, pkgs ? import ../../nix/pkgs.nix { inherit sources; }
}:

let
  mkVirtualisationTestScript = pkgs.callPackage ./mkVirtualisationTestScript.nix { };
  testDirEntries = builtins.readDir ./deployments;
  individualTests = pkgs.lib.mapAttrs'
    (path: _:
      let name = builtins.baseNameOf (pkgs.lib.removeSuffix ".nix" path);
      in
      pkgs.lib.nameValuePair name
        (mkVirtualisationTestScript {
          inherit name;
          deployment = ./deployments + "/${path}";
        }))
    (pkgs.lib.filterAttrs
      (path: type: type == "regular" && pkgs.lib.hasSuffix ".nix" path)
      testDirEntries);
  allTests =
    pkgs.symlinkJoin
      {
        name = "declops-virtualisation-integration-tests";
        paths = builtins.attrValues individualTests;
      };
in
individualTests // { all = allTests; }
