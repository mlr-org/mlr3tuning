lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]$", full.names = TRUE), source)

#FIXME: This function should be exported so it can be used for tests in other packages
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

#FIXME: This function should be exported so it can be used for tests in other packages
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
  term = trm("evals", n_evals = term_evals)
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), term, ps)
  tuner = tnr(key, ...)
  expect_tuner(tuner)
  tuner$optimize(inst)
  archive = inst$archive

  expect_data_table(archive$data(), nrows = real_evals)
  expect_equal(inst$archive$n_evals, real_evals)

  x_opt = inst$result_x_domain
  y_opt = inst$result_y
  expect_list(x_opt, len = n_dim)
  if (n_dim == 1)
    expect_named(x_opt, c("cp"))
  else
    expect_named(x_opt, c("cp", "minsplit"))
  expect_numeric(y_opt, len = 1L)
  expect_named(y_opt, "classif.ce")
  list(tuner = tuner, inst = inst)
}

# test an implemented subclass tuner by running a test with dependent params
# returns: tune_result and instance
test_tuner_dependencies = function(key, ..., term_evals = 2L) {
  term = trm("evals", n_evals = term_evals)
  ll = LearnerRegrDepParams$new()
  inst = TuningInstanceSingleCrit$new(tsk("boston_housing"), ll, rsmp("holdout"), msr("regr.mse"), term, ll$param_set)
  tuner = tnr(key, ...)
  expect_tuner(tuner)
  tuner$optimize(inst)
  archive = inst$archive

  expect_data_table(archive$data(), nrows = term_evals)
  expect_equal(inst$archive$n_evals, term_evals)

  x_opt = inst$result_x_domain
  y_opt = inst$result_y
  expect_list(x_opt)
  expect_names(names(x_opt), subset.of = c("xx", "yy", "cp"))
  expect_numeric(y_opt, len = 1L)
  expect_numeric(y_opt, len = 1L)
  expect_names(names(y_opt), identical.to = "regr.mse")
  list(tuner = tuner, inst = inst)
}


# create a simple inst object for rpart with cp param and 2CV resampling
TEST_MAKE_PS1 = function(n_dim = 1L) {
  if (n_dim == 1) {
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
TEST_MAKE_INST1 = function(values = NULL, folds = 2L,
  measure = msr("classif.ce"), n_dim = 1L, term_evals = 5L, ...) {
  ps = TEST_MAKE_PS1(n_dim = n_dim)
  lrn = mlr_learners$get("classif.rpart")
  if (!is.null(values)) {
    lrn$param_set$values = values
  }
  rs = rsmp("cv", folds = folds)
  term = trm("evals", n_evals = term_evals)
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn, rs, measure, term, ps, ...)
  return(inst)
}

TEST_MAKE_INST1_2D = function(values = NULL, folds = 2L,
  measures = msrs(c("classif.ce", "classif.acc")),  n_dim = 1L, term_evals = 5L,
  ...) {
  ps = TEST_MAKE_PS1(n_dim = n_dim)
  lrn = mlr_learners$get("classif.rpart")
  if (!is.null(values)) {
    lrn$param_set$values = values
  }
  rs = rsmp("cv", folds = folds)
  term = trm("evals", n_evals = term_evals)
  inst = TuningInstanceMultiCrit$new(tsk("iris"), lrn, rs, measures, term, ps,
    ...)
  return(inst)
}

# create inst object with dependencies
TEST_MAKE_PS2 = function() {
  ps = ParamSet$new(
    params = list(
      ParamFct$new("xx", levels = c("a", "b"), default = "a"),
      ParamDbl$new("yy", lower = 0, upper = 1),
      ParamDbl$new("cp", lower = 0, upper = 1)
    )
  )
  ps$add_dep("yy", on = "xx", cond = CondEqual$new("a"))
  return(ps)
}
TEST_MAKE_INST2 = function(measure = msr("dummy.cp.regr"), term_evals = 5L) {
  ps = TEST_MAKE_PS2()
  ll = LearnerRegrDepParams$new()
  rs = rsmp("holdout")
  term = trm("evals", n_evals = term_evals)
  inst = TuningInstanceSingleCrit$new(tsk("boston_housing"), ll, rs, measure, term, ps)
  return(inst)
}

# a dummy measure which simply returns the cp value of rpart
# this allows us to 'fake' performance values in unit tests during tuning
make_dummy_cp_measure = function(type, minimize = TRUE) {
  if (type == "classif") {
    id = "dummy.cp.classif"
    inh = MeasureClassif
    cl = "MeaureDummyCPClassif"
  } else {
    id = "dummy.cp.regr"
    inh = MeasureRegr
    cl = "MeaureDummyCPRegr"
  }
  m = R6Class(cl,
    inherit = inh,
    public = list(
      # allow a fun to transform cp to score, this allows further shenenigans
      # to disentangle cp value and score
      fun = NULL,

      initialize = function(fun = identity) {
        super$initialize(
          id = id,
          range = c(0, Inf),
          minimize = minimize,
          properties = "requires_learner"
        )
        self$fun = fun # allow a fun to transform cp to score
      }
    ),

    private = list(
      .score = function(prediction, learner, ...) {
        self$fun(learner$param_set$values)
      }
    )
  )
}
MeasureDummyCPClassif = make_dummy_cp_measure("classif")
mlr3::mlr_measures$add("dummy.cp.classif", MeasureDummyCPClassif)
MeasureDummyCPRegr = make_dummy_cp_measure("regr")
mlr3::mlr_measures$add("dummy.cp.regr", MeasureDummyCPRegr)
MeasureDummyCPMaximizeClassif = make_dummy_cp_measure("classif", minimize = FALSE)
mlr3::mlr_measures$add("dummy.cp.maximize.classif", MeasureDummyCPMaximizeClassif)

LearnerRegrDepParams = R6Class("LearnerRegrDepParams", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.depparams") {
      param_set = TEST_MAKE_PS2()
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = c("response"),
        param_set = param_set,
        properties = c("missings")
      )
    }
  ),

  private = list(
    .train = function(task) {
      tn = task$target_names
      return(list())
    },

    .predict = function(task) {
      n = task$nrow
      response = rep(99, n)
      PredictionRegr$new(task, response = response)
    }
  )
)

MAKE_GL = function() {
  g = mlr3pipelines::Graph$new()
  op_ds = mlr3pipelines::PipeOpSubsample$new()
  op_lrn = mlr3pipelines::PipeOpLearner$new(lrn("classif.rpart"))
  g$add_pipeop(op_ds)
  g$add_pipeop(op_lrn)
  g$add_edge("subsample", "classif.rpart")
  mlr3pipelines::GraphLearner$new(g)
}

# mlr3::mlr_learners$add("regr.depparams", LearnerRegrDepParams)
