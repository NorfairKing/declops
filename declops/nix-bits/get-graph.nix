{
  # Path to the deployment file
  deployment
, lib ? (deployment.pkgs or (import (import ./nixpkgs-pin.nix) { })).lib
}:


# Input:
#
# {
#   resources = {
#     provider-name = {
#       resource-name = {
#         base = "/tmp";
#         template = "bar";
#       };
#       other-resource-name = {
#         dependencies = [ "provider-name.resource-name" ];
#         spec = { resources }: {
#           base = resources.provider-name.resource-name.path;
#           template = "bar";
#         };
#       };
#     };
#   }
# }
#
#
# Output:
#
#
# {
#   resources = {
#     provider-name = {
#       resource-name = [];
#       other-resource-name = [ "provider-name.resource-name" ];
#     };
#   }
# }
# 
with lib;
let
  dependencies = mapAttrs
    (provider_name: resources:
      mapAttrs
        (resource_name: spec_or_dep:
          spec_or_dep.dependencies or [ ])
        resources
    )
    ((import deployment).resources);
in
{
  inherit dependencies;
}
