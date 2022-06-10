{
  resources = {
    temporary-directory = {
      my-temp-dir = { };
      my-other-temp-dir = { };
    };
    temporary-file = {
      my-temp-file = {
        template = "foo";
        contents = "bar";
      };
      my-other-temp-file = {
        template = "quux";
        contents = "mu";
      };
    };
  };
}
