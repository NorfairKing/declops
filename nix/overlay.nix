final: previous:
with final.lib;
with final.haskell.lib;

let
  declopsPkg = name:
    overrideCabal
      (
        final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}"))
          "--no-hpack"
          { }
      )
      (old: {
        configureFlags = (old.configureFlags or [ ]) ++ [
          # Optimisations
          "--ghc-options=-O2"
          # Extra warnings
          "--ghc-options=-Wall"
          "--ghc-options=-Wincomplete-uni-patterns"
          "--ghc-options=-Wincomplete-record-updates"
          "--ghc-options=-Wpartial-fields"
          "--ghc-options=-Widentities"
          "--ghc-options=-Wredundant-constraints"
          "--ghc-options=-Wcpp-undef"
          "--ghc-options=-Werror"
        ];
        doBenchmark = true;
        doHaddock = false;
        doCoverage = false;
        doHoogle = false;
        doCheck = false; # Only check the release version.
        hyperlinkSource = false;
        enableLibraryProfiling = false;
        enableExecutableProfiling = false;
        buildDepends = (old.buildDepends or [ ]) ++ (with final; [
          haskellPackages.autoexporter
        ]);
        # Ugly hack because we can't just add flags to the 'test' invocation.
        # Show test output as we go, instead of all at once afterwards.
        testTarget = (old.testTarget or "") + " --show-details=direct";
      });
  declopsPkgWithComp =
    exeName: name:
    generateOptparseApplicativeCompletion exeName (declopsPkg name);
  declopsPkgWithOwnComp = name: declopsPkgWithComp name name;
  declopsNixTestPkg = name:
    overrideCabal (declopsPkg name) (old: {
      # To make sure that the test suite is built.
      doCheck = true;
      # To make sure that no tests are run.
      # They will be run in 'declopsNixTest' below.
      testFlags = (old.testFlags or [ ]) ++ [ "--match=nomatchfoobarquux" ];
      # To make sure that the test suite executable is available in the build
      # result
      postInstall = (old.postInstall or "") + ''
        mkdir -p $out/bin/test-suites
        cp dist/build/*-test/*-test $out/bin/test-suites
      '';
    });

in
{
  declopsNixTestPackages = {
    "declops-nix-test" = declopsNixTestPkg "declops-nix-test";
  };
  declopsNixTest = final.callPackage ./nix-test.nix { };

  declopsHaskellPackages =
    {
      "declops" = declopsPkgWithOwnComp "declops";
      "declops-provider" = declopsPkg "declops-provider";
      "declops-provider-gen" = declopsPkg "declops-provider-gen";
      "declops-provider-local" = declopsPkg "declops-provider-local";
      "declops-provider-test" = declopsPkg "declops-provider-test";
      "declops-provider-virtualbox" = declopsPkg "declops-provider-virtualbox";
    };

  declopsReleasePackages = mapAttrs (_: pkg: justStaticExecutables (doCheck pkg)) final.declopsHaskellPackages;
  declopsRelease =
    final.symlinkJoin {
      name = "declops-release";
      paths = attrValues final.declopsReleasePackages;
    };
  declops = final.declopsRelease;


  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super: final.declopsHaskellPackages
            );
      }
    );
}
