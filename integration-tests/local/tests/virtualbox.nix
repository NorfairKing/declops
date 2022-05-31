{
  resources = {
    virtualbox.my-box = {
      name = "foobar-vm";
      base-folder = "/tmp/virtualbox";
    };
  };
}
# TODO this should pass too but it cannot because we destroy in the wrong order.
# {
#   resources = {
#     temporary-directory.my-temp-dir = {
#       base = "/tmp";
#       template = "virtualbox";
#     };
#     virtualbox.my-box = {
#       dependencies = [ "temporary-directory.my-temp-dir" ];
#       spec = { resources }: {
#         name = "foobar-vm";
#         base-folder = resources.temporary-directory.my-temp-dir.path;
#       };
#     };
#   };
# }
