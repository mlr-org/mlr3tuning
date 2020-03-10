context("TunerIrace")
library(mlr3learners)
set.seed(1)

skip_if_not_installed("irace")

test_that("TunerIrace", {
  test_tuner("irace", term_evals = 38)
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
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
                            ps, term("clock_time", secs = 10))
  tt = tnr("irace")
  tt$tune(inst)
  d = inst$archive(unnest = "params")
  expect_integer(d$minsplit)
})

test_that("TunerIrace with dependencies",{
  ps = ParamSet$new(params = list(
    ParamDbl$new("cost", lower = 0, upper = 1),
    ParamFct$new("kernel", levels = c("linear","polynomial"))
  ))
  ps$add_dep("cost","kernel",CondEqual$new("polynomial"))
  inst = TuningInstance$new(tsk("iris"), lrn("classif.svm",type="C-classification"),
                            rsmp("holdout"), msr("classif.ce"), ps, term("clock_time", secs = 8))
  tt = tnr("irace")
  # should it possible to return kernel = linear...?
  expect_silent(tt$tune(inst))
})

test_that("minimize time",{
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))
  te = term("clock_time", secs = 11)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, te)
  tt = tnr("irace", capping = 1, boundMax = 1, cappingType = "best", boundType= "instance")
  tt$tune(inst)
  expect_double(inst$archive(unnest = "params")$minsplit)
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
                          conditions = list(a = TRUE, b = expression(a == TRUE), c = expression(b %in% c(2,5,7))),
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
                          domain = list(a = c("TRUE", "FALSE"), b = c(1,9), c = c("lvl1","lvl2"), d = c(0,1)),
                          conditions = list(c = TRUE, a = expression(c == "lvl1"),
                                            d = expression(c %in% c('lvl1', 'lvl2')),b = expression(a == TRUE)),
                          hierarchy = c(2,3,1,2))
})

test_that("TunerIrace works with logical params", {
  ps = ParamSet$new(params = list(
    ParamLgl$new("intercept")
  ))
  inst = TuningInstance$new(tsk("mtcars"), lrn("regr.glmnet"), rsmp("holdout"), msr("regr.mse"),
                            ps, term("clock_time", secs = 10))
  tt = tnr("irace")
  expect_warning(tt$tune(inst))
  d = inst$archive(unnest = "params")
  expect_logical(d$intercept)
})

test_that("TunerIrace works with tune.threshold", {
  ps = ParamSet$new(params = list(
    ParamInt$new("minsplit", lower = 1, upper = 3)
  ))
  te = term("evals", n_evals = 50)
  tt = tnr("irace", nbIterations = 1L, minNbSurvival = 1)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout", ratio = 0.1), msr("classif.ce"), ps, te)
  expect_silent(tt$tune(inst))
})

test_that("TunerIrace uses digits", {
  te = term("evals", n_evals = 30)
  tt = tnr("irace", nbIterations = 1L, minNbSurvival = 1)
  ps = ParamSet$new(params = list(
    ParamDbl$new("cost", lower = pi * 1e-20,upper = 5.242e12)
  ))
  inst = TuningInstance$new(tsk("iris"), lrn("classif.svm", type="C-classification"), rsmp("holdout"), msr("classif.ce"), ps, te)
  expect_silent(tt$tune(inst))


  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 1e-5, upper = 1e-4)
  ))
  te = term("evals", n_evals = 40)
  tt = tnr("irace", digits = 3L)
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, te)
  expect_error(tt$tune(inst))
})

test_that("Error in hyperparameter tuning with scientific notation for lower/upper boundaries", {
  ps = ParamSet$new(params = list(
    ParamDbl$new("cp", lower = 1e-5, upper = 1e-4)
  ))
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, term("evals", n_evals = 30))
  tt = tnr("irace", nbIterations = 1L, digits = 6L)
  expect_silent(tt$tune(inst))
})

# we had a bug here, see (mlr) issue #627
test_that("irace works with unnamed discrete values", {
  ps = ParamSet$new(params = list(
    ParamInt$new("minsplit", lower = 2L, upper = 7L)
  ))
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
                            ps, term("evals", n_evals = 50))
  tt = tnr("irace")
  expect_silent(tt$tune(inst))
})

# shouldn't paradox check this?
test_that("irace handles parameters with unsatisfiable requirement gracefully", {
  skip_on_os("windows")
  tt = tnr("irace", nbIterations = 1L, minNbSurvival = 1L)

  ps = ParamSet$new(params = list(
    ParamDbl$new("cost", lower = 0, upper = 1),
    ParamFct$new("kernel", levels = c("linear","polynomial"))
  ))
  ps$add_dep("kernel","cost",CondEqual$new(-1)) # cost never feasible
  inst = TuningInstance$new(tsk("iris"), lrn("classif.svm", type= "C-classification"), rsmp("holdout"), msr("classif.ce"),
                            ps, term("evals", n_evals = 50))
  expect_silent(suppressWarnings(tt$tune(inst)))
})
