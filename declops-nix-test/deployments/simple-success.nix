{
  resources = {
    temporary-directory = {
      my-temp-dir = {
        base = "/tmp";
        template = "bar";
      };
      my-other-temp-dir = {
        dependencies = [ "temporary-directory.my-temp-dir" ];
        spec = { resources }: {
          base = resources.temporary-directory.my-temp-dir.path;
          template = "bar";
        };
      };
    };
    temporary-file = {
      my-temp-file = {
        dependencies = [ "temporary-directory.my-temp-dir" ];
        spec = { resources }: {
          base = resources.temporary-directory.my-temp-dir.path;
          template = "foo";
          contents = "bar";
        };
      };
      my-other-temp-file = {
        dependencies = [ "temporary-directory.my-other-temp-dir" ];
        spec = { resources }: {
          base = resources.temporary-directory.my-other-temp-dir.path;
          template = "quux";
          contents = "mu";
        };
      };
    };
  };
}
