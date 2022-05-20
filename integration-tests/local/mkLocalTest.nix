{ nixosTest
}:
{ name
, deployment
}:
nixosTest (
  { lib, pkgs, ... }: {
    name = "declops-integration-test-${name}";
    machine = {
      environment.systemPackages = [
        pkgs.declops
      ];
    };
    testScript = ''
      machine.start()
      machine.wait_for_unit("multi-user.target")

      machine.succeed("declops --help")
      machine.succeed("declops query -d ${deployment}")
      machine.fail("declops check -d ${deployment}")
      machine.succeed("declops apply -d ${deployment}")
      machine.succeed("declops query -d ${deployment}")
      machine.succeed("declops check -d ${deployment}")
      machine.succeed("declops destroy -d ${deployment}")
      machine.succeed("declops query -d ${deployment}")
      machine.fail("declops check -d ${deployment}")
    '';
  }
)
