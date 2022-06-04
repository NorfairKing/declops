{ writeShellScript
, declops
}:
{ name
, deployment
}:
writeShellScript "declops-virtualisation-integration-test-script-${name}" ''
  export PATH="$PATH:${declops}/bin"
  export DECLOPS_LOG_LEVEL=Debug
  export DECLOPS_DEPLOYMENT_FILE="${deployment}"

  succeed () {
    if ! "$@";
    then
      exit 1
    fi
  }
  fail () {
    if "$@";
    then
      exit 1
    fi
  }

  succeed declops --help
  succeed declops query
  fail    declops check
  succeed declops apply
  succeed declops query
  succeed declops check
  succeed declops destroy
  succeed declops query
  fail    declops check
''
