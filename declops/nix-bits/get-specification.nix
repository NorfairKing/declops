# Get the specification of a resource given the outputs of its dependencies
{
  # Path to the deployment file
  deploymentFile

, # Path to the json file with the dependencies' outputs
  outputsFile

, # Provider name of the resource to get the specification of
  providerName

, # Resource name of the resource to get the specification of
  resourceName

, lib ? ((import deploymentFile).pkgs or (import (import ./nixpkgs-pin.nix) { })).lib
}:


with lib;
let
  deployment = import deploymentFile;
  outputs = builtins.fromJSON (builtins.readFile outputsFile);
  resourceDefinition = deployment.resources."${providerName}"."${resourceName}";
  output =
    if builtins.hasAttr "spec" resourceDefinition
    # It has dependencies, this will be a function
    then
      resourceDefinition.spec { resources = outputs; }
    # It has no dependencies, this will just be a value
    else
      resourceDefinition;
in
{ inherit output; }
