{
  resources = {
    temporary-directory.my-temp-dir = { };
    virtualbox.my-box = {
      dependencies = [ "temporary-directory.my-temp-dir" ];
      spec = { resources }: {
        name = "foobar-vm";
        running = true;
        base-folder = resources.temporary-directory.my-temp-dir.path;
      };
    };
  };
}
