{
  resources = {
    temporary-directory = {
      my-temp-dir = {
        base = "/tmp";
        template = "bar";
      };
    };
    unknown-provider = {
      foo = { };
    };
    other-unknown-provider = {
      bar = { };
    };
  };
}
