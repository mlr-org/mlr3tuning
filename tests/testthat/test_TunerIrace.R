skip_if_not_installed("irace")

test_that("TunerIrace", {
  z = test_tuner("irace", term_evals = 42, real_evals = 39)
  a = z$inst$archive$data
  expect_subset(c("id_configuration", "id_instance"), names(a))

  # check for mean performance
  expect_equal(z$inst$result$classif.ce, mean(a[id_configuration == z$inst$result$id_configuration,]$classif.ce))
})

test_that("TunerIrace works with TerminatorRunTime", {
  search_space = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_int(lower = 1, upper = 10)
  )
  instance = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
    trm("run_time", secs = 30), search_space)
  tuner = tnr("irace")
  expect_data_table(tuner$optimize(instance))
})

test_that("TunerIrace with unsupported terminators", {
   search_space = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_int(lower = 1, upper = 10)
  )
  instance = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
    trm("run_time", secs = 30), search_space)
  tuner = tnr("irace")
})

test_that("TunerIrace works with dependencies", {
  search_space = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_int(lower = 1, upper = 10, depends = cp == 0.005)
  )
  instance = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
    trm("evals", n_evals = 96), search_space)
  tuner = tnr("irace")
  tuner$optimize(instance)
  a = instance$archive$data
  expect_true(all(is.na(a[cp != 0.005, minsplit])))
  expect_double(a$cp)
})

test_that("paradox_to_irace without dependencies", {
  # only ParamLgl
  pps = ps(lgl = p_lgl())
  expect_irace_parameters(parameters = paradox_to_irace(pps), names = "lgl", types = "c", 
    domain = list(lgl = c("TRUE", "FALSE")), conditions = list(lgl = TRUE))
  
  # only ParamUty
  pps = ps(uty = p_uty())
  expect_error(paradox_to_irace(pps), regex = "<ParamUty> not supported by <TunerIrace>", fixed = TRUE)

  # mixed set
  pps = ps(
    dbl = p_dbl(lower = 0.1, upper = 0.3),
    int = p_int(lower = 1, upper = 9),
    fct = p_fct(levels = c("a", "b", "c")),
    lgl = p_lgl()
  )
  expect_irace_parameters(
    parameters = paradox_to_irace(pps),
    names = c("dbl", "int", "fct", "lgl"),
    types = c("r", "i", "c", "c"),
    domain = list(dbl = c(0.1, 0.3), int = c(1, 9), fct = c("a", "b", "c"), lgl = c("TRUE", "FALSE")))

  # double checking previous bug in merge sort
  pps = ps(
    fct = p_fct(levels = c("a", "b", "c")),
    int1 = p_int(lower = 1, upper = 9),
    dbl = p_dbl(lower = 0.1, upper = 0.3),
    int2 = p_int(lower = 10, upper = 90),
    lgl = p_lgl()
  )
  expect_irace_parameters(
    parameters = paradox_to_irace(pps),
    names = c("fct", "int1", "dbl", "int2", "lgl"),
    types = c("c", "i", "r", "i", "c"),
    domain = list(fct = c("a", "b", "c"), int1 = c(1, 9), dbl = c(0.1, 0.3), int2 = c(10, 90), 
      lgl = c("TRUE", "FALSE")))
})

test_that("paradox_to_irace with dependencies", {
  # one dependency
  pps = ps(
    a = p_lgl(),
    b = p_int(lower = 1, upper = 9, depends = a == TRUE)
  )
  expect_irace_parameters(
    parameters = paradox_to_irace(pps), names = c("a", "b"), types = c("c", "i"),
    domain = list(a = c("TRUE", "FALSE"), b = c(1, 9)),
    conditions = list(a = TRUE, b = expression(a == TRUE)),
    depends = list(a = character(0), b = "a"),
    hierarchy = c(1, 2))

 # two dependencies
  pps = ps(
    a = p_lgl(),
    c = p_fct(levels = c("lvl1", "lvl2"), depends = b %in% c(2, 5, 7)),
    b = p_int(lower = 1, upper = 9, depends = a == TRUE)
  )
  expect_irace_parameters(
    parameters = paradox_to_irace(pps), names = c("a", "c", "b"),
    types = c("c", "c", "i"),
    domain = list(a = c("TRUE", "FALSE"), c = c("lvl1", "lvl2"), b = c(1, 9)),
    conditions = list(
      a = TRUE, b = expression(a == TRUE),
      c = expression(b %in% c(2, 5, 7))),
    depends = list(a = character(0), c = "b", b = "a"),
    hierarchy = c(1, 3, 2))

  # three dependencies
  pps = ps(
    a = p_lgl(depends = c == "lvl1"),
    b = p_int(lower = 1, upper = 9, depends = a == TRUE),
    c = p_fct(levels = c("lvl1", "lvl2")),
    d = p_dbl(lower = 0, upper = 1, depends = c %in% c("lvl1", "lvl2"))
  )
  expect_irace_parameters(
    parameters = paradox_to_irace(pps), names = c("a", "b", "c", "d"),
    types = c("c", "i", "c", "r"),
    domain = list(
      a = c("TRUE", "FALSE"), b = c(1, 9), c = c("lvl1", "lvl2"),
      d = c(0, 1)),
    conditions = list(
      c = TRUE, a = expression(c == "lvl1"),
      d = expression(c %in% c("lvl1", "lvl2")),
      b = expression(a == TRUE)),
    depends = list(a = "c", b = "a", c = character(0), d = "c"),
    hierarchy = c(2, 3, 1, 2))
})

test_that("TunerIrace works with logical parameters", {
  pps = ps(keep_model = p_lgl())
  instance = TuningInstanceSingleCrit$new(tsk("mtcars"), lrn("regr.rpart"), rsmp("holdout"), msr("regr.mse"),
    trm("evals", n_evals = 42), pps)
  tuner = tnr("irace")
  tuner$optimize(instance)
  expect_logical(instance$archive$best()$keep_model)
})

test_that("TunerIrace uses digits", {
  pps = ps(cp = p_dbl(lower = pi * 1e-20, upper = 5.242e12 / 1e13))
  instance = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), 
  trm("evals", n_evals = 30), pps)
  tuner = tnr("irace", nbIterations = 1L, minNbSurvival = 1)
  expect_data_table(tuner$optimize(instance))
})

test_that("irace works with unnamed discrete values", {
  # we had a bug here, see (mlr) issue #627
  pps = ps(minsplit = p_int(lower = 2L, upper = 7L))
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
    trm("evals", n_evals = 50), pps)
  tuner = tnr("irace")
  expect_data_table(tuner$optimize(inst))
})
