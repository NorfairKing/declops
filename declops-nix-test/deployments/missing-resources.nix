{
  resources = {
    temporary-file = {
      my-temp-file = {
        dependencies = [ "temporary-directory.my-temp-dir" "temporary-directory.my-other-temp-dir" ];
        spec = { resources }: {
          base = resources.temporary-directory.my-temp-dir.path;
          template = "foo";
          contents = "bar";
        };
      };
    };
  };
}
