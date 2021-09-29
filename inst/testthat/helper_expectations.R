expect_tuner = function(tuner) {
  expect_r6(tuner, "Tuner",
    public = c("optimize", "param_set"),
    private = ".optimize"
  )
  expect_class(tuner$param_set, "ParamSet")
  expect_function(tuner$optimize, args = "inst")
}

expect_terminator = function(term) {
  expect_r6(term, "Terminator",
    public = c("is_terminated", "param_set")
  )
  expect_class(term$param_set, "ParamSet")
}
