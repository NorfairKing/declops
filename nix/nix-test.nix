{ nixosTest
, declopsNixTestPackages
}:
nixosTest (
  { lib, pkgs, ... }: {
    name = "declops-nix-test";
    machine = {
      environment.systemPackages = with pkgs; [
        nix
        git
      ];
    };
    testScript = ''
      machine.start()
      machine.wait_for_unit("multi-user.target")

      machine.succeed("${declopsNixTestPackages.declops-nix-test}/bin/test-suites/declops-nix-test-test")
    '';
  }
)
