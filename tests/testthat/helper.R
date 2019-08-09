lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_terminator = function(term) {
  expect_r6(term, "Terminator", public = c("eval_before", "eval_after"))
  expect_flag(term$is_terminated)
}

# test an implemented subclass tuner by running a couple of standard tests
# on a simple example
# term_evals: how we configure the Terminator
# real_evals: how many evals we really expect (as the optim might early stop)
# returns: tuner, so we can investgate its state more in individual tests
test_tuner = function(tuner_factory, arg_list = list(), n_dim = 1L, term_evals = 2L, real_evals = term_evals) {

  ps = if (n_dim == 1) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3)
    ))
  } else if (n_dim == 2) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3),
      ParamInt$new("minsplit", lower = 1, upper = 9)
    ))
  }
  term = TerminatorEvals$new(term_evals)
  pe = PerfEval$new("iris", "classif.rpart", "holdout", "classif.ce", ps, term)
  tuner = do.call(tuner_factory$new, arg_list)
  tuner$pe = pe

  tuner$tune()
  r = tuner$tune_result()
  bmr = tuner$pe$bmr

  expect_data_table(bmr$data, nrows = real_evals)
  expect_equal(pe$n_evals, real_evals)
  expect_list(r)
  expect_number(r$performance["classif.ce"], lower = 0, upper = 1)
  expect_list(r$values, len = n_dim + 1)
  return(tuner)
}

# create a simple PE object for rpart with cp param and 2CV resampling
TEST_MAKE_PS1 = function(n_dim = 1L) {
  ps = if (n_dim == 1) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3)
    ))
  } else if (n_dim == 2) {
    ParamSet$new(params = list(
      ParamDbl$new("cp", lower = 0.1, upper = 0.3),
      ParamInt$new("minsplit", lower = 1, upper = 9)
    ))
  }
}
TEST_MAKE_PE1 = function(values = NULL, folds = 2L, measures = "classif.ce", n_dim = 1L, term_evals = 5L) {
  ps = TEST_MAKE_PS1(n_dim = n_dim)
  lrn = mlr_learners$get("classif.rpart")
  if (!is.null(values))
    lrn$param_set$values = values
  rs = mlr_resamplings$get("cv", param_vals = list(folds = folds))
  term = TerminatorEvals$new(term_evals)
  pe = PerfEval$new("iris", lrn, rs, measures, ps, term)
  return(pe)
}


# a dummy measure which simply returns the cp value of rpart
# this allows us to 'fake' performance values in unit tests during tuning
MeasureDummyCP = R6Class("MeasureDummyCP",
  inherit = MeasureClassif,
  public = list(
    initialize = function() {
      super$initialize(
        id = "dummy.cp",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "Metrics",
        properties = "requires_learner"
      )
    },

    score_internal = function(prediction, learner, ...) {
      learner$param_set$values$cp
    }
  )
)
mlr3::mlr_measures$add("dummy.cp", MeasureDummyCP)
