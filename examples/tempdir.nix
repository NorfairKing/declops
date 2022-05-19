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
    other-thing-that-requires-other-resources = {
      foo = { resources }: { };
    };
  };
}
