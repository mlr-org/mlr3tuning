lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)

expect_tuner = function(tuner) {
  expect_r6(tuner, "Tuner",
    public = c("tune", "param_set"),
    private = "tune_internal"
  )
  expect_is(tuner$param_set, "ParamSet")
  expect_function(tuner$tune, args = "instance")
}

expect_terminator = function(term) {
  expect_r6(term, "Terminator",
    public = c("is_terminated", "param_set")
  )
  expect_is(term$param_set, "ParamSet")
}

# test an implemented subclass tuner by running a couple of standard tests
# on a simple example
# term_evals: how we configure the Terminator
# real_evals: how many evals we really expect (as the optim might early stop)
# returns: tune_result and instance
test_tuner = function(key, ..., n_dim = 1L, term_evals = 2L, real_evals = term_evals) {
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
  inst = TuningInstance$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), ps, term)
  tuner = tnr(key, ...)

  expect_tuner(tuner$tune(inst))
  # r = tuner$tune_result(inst)
  bmr = inst$bmr

  expect_data_table(bmr$data, nrows = real_evals)
  expect_equal(inst$n_evals, real_evals)

  sc = inst$result_config
  sp = inst$result_perf
  expect_list(sc, len = n_dim + 1)
  if (n_dim == 1)
    expect_named(sc, c("xval", "cp"))
  else
    expect_named(sc, c("xval", "cp", "minsplit"))
  expect_numeric(sp, len = 1L)
  expect_named(sp, "classif.ce")
  list(tuner = tuner, inst = inst)
}

# test an implemented subclass tuner by running a test with dependent params
# returns: tune_result and instance
test_tuner_dependencies = function(key, ..., n_evals = 2L) {
  term = TerminatorEvals$new(n_evals)
  ll = LearnerRegrDepParams$new()
  inst = TuningInstance$new(tsk("boston_housing"), ll, rsmp("holdout"), msr("regr.mse"), ll$param_set, term)
  tuner = tnr(key, ...)

  expect_tuner(tuner$tune(inst))
  # r = tuner$tune_result(inst)
  bmr = inst$bmr

  expect_data_table(bmr$data, nrows = n_evals)
  expect_equal(inst$n_evals, n_evals)


  sc = inst$result_config
  sp = inst$result_perf
  expect_list(sc)
  expect_names(names(sc), subset.of = c("p1", "p2"))
  expect_numeric(sp, len = 1L)
  expect_numeric(sp, len = 1L)
  expect_names(names(sp), identical.to = "regr.mse")
  list(tuner = tuner, inst = inst)
}


# create a simple inst object for rpart with cp param and 2CV resampling
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
TEST_MAKE_INST1 = function(values = NULL, folds = 2L, measures = msr("classif.ce"), n_dim = 1L, term_evals = 5L) {
  ps = TEST_MAKE_PS1(n_dim = n_dim)
  lrn = mlr_learners$get("classif.rpart")
  if (!is.null(values)) {
    lrn$param_set$values = values
  }
  rs = rsmp("cv", folds = folds)
  term = term("evals", n_evals = term_evals)
  inst = TuningInstance$new(tsk("iris"), lrn, rs, measures, ps, term)
  return(inst)
}


# a dummy measure which simply returns the cp value of rpart
# this allows us to 'fake' performance values in unit tests during tuning
MeasureDummyCP = R6Class("MeasureDummyCP",
  inherit = MeasureClassif,
  public = list(
    # allow a fun to transform cp to score, this allows further shenenigans
    # to disentangle cp value and score
    fun = NULL,

    initialize = function(fun = identity) {
      super$initialize(
        id = "dummy.cp",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "Metrics",
        properties = "requires_learner"
      )
      self$fun = fun # allow a fun to transform cp to score
    },

    score_internal = function(prediction, learner, ...) {
      self$fun(learner$param_set$values$cp)
    }
  )
)
mlr3::mlr_measures$add("dummy.cp", MeasureDummyCP)


LearnerRegrDepParams = R6Class("LearnerRegrDepParams", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.depparams") {
      param_set = ParamSet$new(
          params = list(
            ParamFct$new("p1", levels = c("a", "b"), default = "a"),
            ParamDbl$new("p2", lower = 0, upper = 1)
          )
      )
      param_set$add_dep("p2", on = "p1", cond = CondEqual$new("a"))
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c("missings")
      )
    },

    train_internal = function(task) {
      tn = task$target_names
      return(list())
    },

    predict_internal = function(task) {
      n = task$nrow
      response = rep(99, n)
      PredictionRegr$new(task, response = response)
    }
  )
)

# mlr3::mlr_learners$add("regr.depparams", LearnerRegrDepParams)
