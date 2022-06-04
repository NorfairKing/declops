let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "release" = pkgs.declopsRelease;
  "pre-commit-check" = pre-commit.run;
  "shell" = pkgs.symlinkJoin {
    name = "shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
  "nix-test" = pkgs.declopsNixTest;
  "virtualisation-test-script" = pkgs.declopsVirtualisationTestScript;
  "local-integration-tests" = (import ./integration-tests/local { inherit sources pkgs; }).all;
  "virtualisation-integration-test-script" = (import ./integration-tests/virtualisation { inherit sources pkgs; }).all;
}
