{ writeShellScript
, declopsVirtualisationTestPackages
}:
writeShellScript "declops-virtualisation-test" ''
  ${declopsVirtualisationTestPackages.declops-provider-virtualbox-test}/bin/test-suites/declops-provider-virtualbox-test
''
