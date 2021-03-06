{ sources ? import ../../nix/sources.nix
, pkgs ? import ../../nix/pkgs.nix { inherit sources; }
}:

let
  mkLocalTest = pkgs.callPackage ./mkLocalTest.nix { };
  testDirEntries = builtins.readDir ./deployments;
  individualTests = pkgs.lib.mapAttrs'
    (path: _:
      let name = builtins.baseNameOf (pkgs.lib.removeSuffix ".nix" path);
      in
      pkgs.lib.nameValuePair name
        (mkLocalTest {
          inherit name;
          deployment = ./deployments + "/${path}";
        }))
    (pkgs.lib.filterAttrs
      (path: type: type == "regular" && pkgs.lib.hasSuffix ".nix" path)
      testDirEntries);
  allTests =
    pkgs.symlinkJoin
      {
        name = "declops-local-integration-tests";
        paths = builtins.attrValues individualTests;
      };
in
individualTests // { all = allTests; }
