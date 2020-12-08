library(mlr3learners)
set.seed(1)

skip_if_not_installed("irace")

test_that("TunerIrace", {
  test_tuner("irace", term_evals = 42, real_evals = 39)
})

test_that("TunerIrace with int params and trafo, clock terminator", {
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamDbl$new("minsplit", lower = 1, upper = 10)
  ))
  ps$trafo = function(x, param_set) {
    x$minsplit = as.integer(round(x$minsplit))
    return(x)
  }
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"),
                                      msr("classif.ce"), trm("run_time", secs = 24), ps)
  tt = tnr("irace")
  tt$optimize(inst)
  expect_double(inst$archive$best()$cp)
})

test_that("TunerIrace with dependencies",{
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))
  ps$add_dep("minsplit","cp",CondEqual$new(0.005))
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"),
                            rsmp("holdout"), msr("classif.ce"),
                            trm("evals", n_evals = 96), ps)
  tt = tnr("irace")
  expect_output(tt$optimize(inst))
})

test_that("minimize time",{
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1)
  ))
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"),
                                      msr("classif.ce"), trm("run_time", secs = 20), ps)
  tt = tnr("irace", capping = 1, boundMax = 1, cappingType = "best", boundType = "instance")
  tt$optimize(inst)
  expect_double(inst$archive$best()$cp)
})

test_that("paradox_to_irace no dependencies",{
  ps = ParamSet$new(params = list(
    ParamLgl$new("lgl")
  ))
  expect_irace_parameters(parameters = paradox_to_irace(ps), names = "lgl", types = "c",
                          domain = list(lgl = c("TRUE", "FALSE")), conditions = list(lgl = TRUE))

  ps = ParamSet$new(params = list(
    ParamUty$new("uty")
  ))
  expect_error(paradox_to_irace(ps))

  ps = ParamSet$new(params = list(
    ParamDbl$new("dbl", lower = 0.1, upper = 0.3),
    ParamInt$new("int", lower = 1, upper = 9),
    ParamFct$new("fct", levels = c("a","b","c")),
    ParamLgl$new("lgl")
  ))
  expect_irace_parameters(parameters = paradox_to_irace(ps),
                          names = c("dbl","int","fct","lgl"),
                          types = c("r","i","c","c"),
                          domain = list(dbl = c(0.1,0.3), int = c(1,9), fct = c("a","b","c"),
                                        lgl = c("TRUE", "FALSE")))

  # double checking previous bug in merge sort
  ps = ParamSet$new(params = list(
    ParamFct$new("fct", levels = c("a","b","c")),
    ParamInt$new("int1", lower = 1, upper = 9),
    ParamDbl$new("dbl", lower = 0.1, upper = 0.3),
    ParamInt$new("int2", lower = 10, upper = 90),
    ParamLgl$new("lgl")
  ))
  expect_irace_parameters(parameters = paradox_to_irace(ps),
                          names = c("fct","int1","dbl","int2","lgl"),
                          types = c("c","i","r","i","c"),
                          domain = list(fct = c("a","b","c"), int1 = c(1,9), dbl = c(0.1,0.3),
                                        int2 = c(10,90), lgl = c("TRUE", "FALSE")))
})

test_that("paradox_to_irace dependencies",{
  ps = ParamSet$new(params = list(
    ParamLgl$new("a"),
    ParamInt$new("b", lower = 1, upper = 9))
  )
  ps$add_dep("b", "a", CondEqual$new(TRUE))
  expect_irace_parameters(parameters = paradox_to_irace(ps), names = c("a","b"), types = c("c","i"),
                          domain = list(a = c("TRUE", "FALSE"), b = c(1,9)),
                          conditions = list(a = TRUE, b = expression(a == TRUE)),
                          depends = list(a = character(0), b = "a"),
                          hierarchy = c(1,2))

  ps = ParamSet$new(params = list(
    ParamLgl$new("a"),
    ParamFct$new("c", levels = c("lvl1","lvl2")),
    ParamInt$new("b", lower = 1, upper = 9)
  ))
  ps$add_dep("b", "a", CondEqual$new(TRUE))
  ps$add_dep("c", "b", CondAnyOf$new(c(2,5,7)))
  expect_irace_parameters(parameters = paradox_to_irace(ps), names = c("a","c","b"),
                          types = c("c","c","i"),
                          domain = list(a = c("TRUE", "FALSE"), c = c("lvl1","lvl2"), b = c(1,9)),
                          conditions = list(a = TRUE, b = expression(a == TRUE),
                                            c = expression(b %in% c(2,5,7))),
                          depends = list(a = character(0),c = "b", b = "a"),
                          hierarchy = c(1,3,2))


  ps = ParamSet$new(params = list(
    ParamLgl$new("a"),
    ParamInt$new("b", lower = 1, upper = 9),
    ParamFct$new("c", levels = c("lvl1","lvl2")),
    ParamDbl$new("d", lower = 0, upper = 1)
  ))
  ps$add_dep("b", "a", CondEqual$new(TRUE))
  ps$add_dep("a", "c", CondEqual$new("lvl1"))
  ps$add_dep("d", "c", CondAnyOf$new(c("lvl1","lvl2")))
  expect_irace_parameters(parameters = paradox_to_irace(ps), names = c("a","b","c","d"),
                          types = c("c","i","c","r"),
                          domain = list(a = c("TRUE", "FALSE"), b = c(1,9), c = c("lvl1","lvl2"),
                                        d = c(0,1)),
                          conditions = list(c = TRUE, a = expression(c == "lvl1"),
                                            d = expression(c %in% c('lvl1', 'lvl2')),
                                            b = expression(a == TRUE)),
                          depends = list(a = "c", b = "a", c = character(0), d = "c"),
                          hierarchy = c(2,3,1,2))
})

test_that("TunerIrace works with logical params", {
  ps = ParamSet$new(params = list(
    ParamLgl$new("keep_model")
  ))
  inst = TuningInstanceSingleCrit$new(tsk("mtcars"), lrn("regr.rpart"), rsmp("holdout"),
                                      msr("regr.mse"),
                            trm("evals", n_evals = 42), ps)
  tt = tnr("irace")
  tt$optimize(inst)
  expect_logical(inst$archive$best()$keep_model)
})

test_that("TunerIrace works with tune.threshold", {
  ps = ParamSet$new(params = list(
    ParamInt$new("minsplit", lower = 1, upper = 3)
  ))
  tt = tnr("irace", nbIterations = 1L, minNbSurvival = 1)
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"),
                                      rsmp("holdout", ratio = 0.1), msr("classif.ce"),
                                      trm("evals", n_evals = 50), ps)
  expect_output(tt$optimize(inst))
})

test_that("TunerIrace uses digits", {
  tt = tnr("irace", nbIterations = 1L, minNbSurvival = 1)
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = pi * 1e-20, upper = 5.242e12/1e13)
  ))
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"),
                                      msr("classif.ce"), trm("evals", n_evals = 30), ps)
  expect_output(tt$optimize(inst))


  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 1e-5, upper = 1e-4)
  ))
  tt = tnr("irace", digits = 3L)
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"),
                                      msr("classif.ce"), trm("evals", n_evals = 40), ps)
  expect_error(tt$optimize(inst))
})

test_that("Error in hyperparameter tuning with scientific notation for lower/upper boundaries", {
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 1e-3, upper = 1e-1)
  ))
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"),
                                      msr("classif.ce"), trm("evals", n_evals = 30), ps)
  tt = tnr("irace", nbIterations = 1L)
  expect_output(tt$optimize(inst))
})

# we had a bug here, see (mlr) issue #627
test_that("irace works with unnamed discrete values", {
  ps = ParamSet$new(params = list(
    ParamInt$new("minsplit", lower = 2L, upper = 7L)
  ))
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
                                      trm("evals", n_evals = 50), ps)
  tt = tnr("irace")
  expect_output(tt$optimize(inst))
})
