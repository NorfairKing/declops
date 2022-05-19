{
  resources = {
    temporary-file = {
      my-temp-file = {
        base = "/tmp";
        template = "foo";
        contents = "bar";
      };
      my-other-temp-file = {
        base = "/tmp";
        template = "quux";
        contents = "mu";
      };
    };
  };
}
