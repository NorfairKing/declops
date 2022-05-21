{
  # Path to the deployment file
  deploymentFile
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
let
  deployment = import deploymentFile;
  dependencies = builtins.mapAttrs
    (_: resources:
      builtins.mapAttrs
        (_: specOrDep:
          specOrDep.dependencies or [ ])
        resources
    )
    deployment.resources;
in
{ inherit dependencies; }
