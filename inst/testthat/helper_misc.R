# create a simple inst object for rpart with cp param and 2CV resampling
TEST_MAKE_PS1 = function(n_dim = 1L) {
  if (n_dim == 1) {
    ps(
      cp = p_dbl(lower = 0.1, upper = 0.3)
    )
  } else if (n_dim == 2) {
    ps(
      cp = p_dbl(lower = 0.1, upper = 0.3),
      minsplit = p_int(lower = 1, upper = 9)
    )
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
  inst = TuningInstanceBatchSingleCrit$new(tsk("iris"), lrn, rs, measure, term, ps, ...)
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
  inst = TuningInstanceBatchMultiCrit$new(tsk("iris"), lrn, rs, measures, term, ps,
    ...)
  return(inst)
}

# create inst object with dependencies
TEST_MAKE_PS2 = function() {
  ps = ps(
    xx = p_fct(levels = c("a", "b"), default = "a"),
    yy = p_dbl(lower = 0, upper = 1),
    cp = p_dbl(lower = 0, upper = 1)
  )
  ps$add_dep("yy", on = "xx", cond = CondEqual$new("a"))
  return(ps)
}
TEST_MAKE_INST2 = function(measure = msr("dummy.cp.regr"), term_evals = 5L) {
  ps = TEST_MAKE_PS2()
  ll = LearnerRegrDepParams$new()
  rs = rsmp("holdout")
  term = trm("evals", n_evals = term_evals)
  inst = TuningInstanceBatchSingleCrit$new(tsk("boston_housing"), ll, rs, measure, term, ps)
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
mlr_measures$add("dummy.cp.classif", MeasureDummyCPClassif)
MeasureDummyCPRegr = make_dummy_cp_measure("regr")
mlr_measures$add("dummy.cp.regr", MeasureDummyCPRegr)
MeasureDummyCPMaximizeClassif = make_dummy_cp_measure("classif", minimize = FALSE)
mlr_measures$add("dummy.cp.maximize.classif", MeasureDummyCPMaximizeClassif)

LearnerRegrDepParams = R6Class("LearnerRegrDepParams", inherit = LearnerRegr,
  public = list(
    initialize = function(id = "regr.depparams") {
      param_set = TEST_MAKE_PS2()
      super$initialize(
        id = id,
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        predict_types = "response",
        param_set = param_set,
        properties = "missings"
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
      list(response = response)
    }
  )
)

MAKE_GL = function() {
  g = Graph$new()
  op_ds = PipeOpSubsample$new()
  op_lrn = PipeOpLearner$new(lrn("classif.rpart"))
  g$add_pipeop(op_ds)
  g$add_pipeop(op_lrn)
  g$add_edge("subsample", "classif.rpart")
  GraphLearner$new(g)
}

flush_redis = function() {
  config = redux::redis_config()
  r = redux::hiredis(config)
  r$FLUSHDB()
}

expect_rush_reset = function(rush, type = "kill") {
  processes = rush$processes
  rush$reset(type = type)
  Sys.sleep(1)
  keys = rush$connector$command(c("KEYS", "*"))
  if (!test_list(keys, len = 0)) {
    stopf("Found keys in redis after reset: %s", keys)
  }
  walk(processes, function(p) p$kill())
}
