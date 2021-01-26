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

expect_irace_parameters = function(parameters, names, types, domain, conditions, depends, hierarchy) {
  expect_list(parameters, len = 12, any.missing = FALSE)
  expect_equal(names(parameters), c("names", "types", "switches", "domain", "conditions", "isFixed", "transform", 
    "depends", "hierarchy", "nbParameters", "nbFixed", "nbVariable"))
  expect_equal(parameters$names, names)
  expect_equal(parameters$types, set_names(types, names))
  expect_equal(parameters$switches, named_vector(names, ""))
  expect_equal(parameters$domain, domain)
  if (missing(conditions)) {
    expect_equal(parameters$conditions, named_list(names, TRUE))
  } else {
    # can't compare expressions directly
    expect_equal(as.character(parameters$conditions), as.character(conditions))
  }
  expect_equal(parameters$isFixed, named_vector(names, FALSE))
  expect_equal(parameters$transform, named_list(names, ""))
  if (missing(depends)) {
    expect_equal(parameters$depends, named_list(names, character(0)))
  } else {
    expect_equal(parameters$depends, depends)
  }
  if (missing(hierarchy)) {
    expect_equal(parameters$hierarchy, named_vector(names, 1))
  } else {
    expect_equal(parameters$hierarchy, set_names(hierarchy, names))
  }
  expect_equal(parameters$nbParameters, length(names))
  expect_equal(parameters$nbFixed, 0)
  expect_equal(parameters$nbVariable, length(names))
}
