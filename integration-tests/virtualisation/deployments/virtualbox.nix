{
  resources = {
    temporary-directory.my-temp-dir = {
      base = "/tmp";
      template = "virtualbox";
    };
    virtualbox.my-box = {
      dependencies = [ "temporary-directory.my-temp-dir" ];
      spec = { resources }: {
        name = "foobar-vm";
        start = false;
        base-folder = resources.temporary-directory.my-temp-dir.path;
      };
    };
  };
}
