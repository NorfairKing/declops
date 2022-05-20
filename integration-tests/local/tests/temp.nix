{
  resources = {
    temporary-directory = {
      my-temp-dir = {
        base = "/tmp";
        template = "bar";
      };
      my-other-temp-dir = {
        base = "/tmp";
        template = "bar";
      };
    };
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
