test_that("API", {
  for (n_evals in c(1, 5)) {
    rs = TunerBatchRandomSearch$new()
    inst = TEST_MAKE_INST1(measure = msr("classif.ce"), term_evals = n_evals)
    expect_data_table(rs$optimize(inst), nrows = 1)
    a = inst$archive$data
    expect_data_table(a, nrows = n_evals)
    expect_true("cp" %in% names(a))
  }
})

test_that("proper error if tuner cannot handle deps", {
  skip_if_not_installed("GenSA")
  ps = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_dbl(lower = 1, upper = 10)
  )
  ps$add_dep("minsplit", on = "cp", cond = CondEqual$new(0.1))
  te = trm("evals", n_evals = 2)
  inst = TuningInstanceBatchSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), te, ps)
  tt = TunerBatchGenSA$new()
  expect_error(tt$optimize(inst), "dependencies")
})

test_that("we get a result when some subordinate params are not fulfilled", {
  TunerManual = R6Class("TunerManual",
    inherit = TunerBatch,
    public = list(
      initialize = function() {
        super$initialize(
          id = "manual",
          param_set = ParamSet$new(),
          param_classes = c("ParamFct", "ParamDbl"),
          properties = c("dependencies", "single-crit")
        )
      }
    ),
    private = list(
      .optimize = function(inst) {
      }
    )
  )
  tuner_manual = TunerManual$new()
  inst = TEST_MAKE_INST2(measure = msr("dummy.cp.regr", fun = function(pv) pv$cp))
  d = data.table(xx = c("a", "b"), yy = c(1, NA), cp = c(0.2, 0.1))
  inst$eval_batch(d)
  tuner_manual$optimize(inst)
  expect_equal(inst$result_y, c(dummy.cp.regr = 0.1))
  expect_equal(inst$result_x_domain, list(xx = "b", cp = 0.1))
  expect_equal(inst$result_x_domain, inst$result_learner_param_vals)
})

test_that("print method workds", {
  param_set = ps(p1 = p_lgl())
  param_set$values$p1 = TRUE
  param_classes = "ParamLgl"
  properties = "single-crit"
  packages = "GenSA"

  tuner = Tuner$new(
    id = "tuner",
    param_set = param_set,
    param_classes = param_classes,
    properties = "single-crit",
    packages = packages)
  expect_snapshot(tuner)
})

test_that("Tuner works with graphlearner", {
  skip_if_not_installed("mlr3pipelines")
  requireNamespace("mlr3pipelines")

  gl = MAKE_GL()
  task = tsk("iris")
  ms = MeasureDummyCPClassif$new(fun = function(pv) if (pv$classif.rpart.cp == 0.2) 0 else 1)
  te = trm("evals", n_evals = 4)
  ps = ps(
    classif.rpart.cp = p_dbl(lower = 0.1, upper = 0.3)
  )
  inst = TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = gl,
    resampling = rsmp("holdout"),
    measure = ms,
    search_space = ps,
    terminator = te)

  tuner = tnr("grid_search", resolution = 3)
  tuner$optimize(inst)
  archive = inst$archive

  expect_data_table(archive$data, nrows = 3)
  expect_equal(inst$archive$n_evals, 3)
  expect_equal(inst$result_x_domain, list(classif.rpart.cp = 0.2))
  expect_equal(inst$result_y, c(dummy.cp.classif = 0))
})

test_that("Tuner works with instantiated resampling", {
  task = tsk("iris")
  resampling = rsmp("custom")
  train_sets = list(1:75)
  test_sets = list(76:150)
  resampling$instantiate(task, train_sets, test_sets)

  expect_true(resampling$is_instantiated)

  te = trm("evals", n_evals = 4)
  ps = ps(
    cp = p_dbl(lower = 0.1, upper = 0.3)
  )
  inst = TuningInstanceBatchSingleCrit$new(
    task = task,
    learner = lrn("classif.rpart"),
    resampling = resampling,
    measure = msr("classif.ce"),
    search_space = ps,
    terminator = te)

  rs = TunerBatchRandomSearch$new()
  rs$optimize(inst)

  expect_r6(inst$objective$resampling, "ResamplingCustom")
  expect_equal(inst$objective$resampling$instance$train[[1]], 1:75)
  expect_equal(inst$objective$resampling$instance$test[[1]], 76:150)
})

test_that("Tuner active bindings work", {
  param_set = ps(p1 = p_lgl())
  param_set$values$p1 = TRUE
  param_classes = "ParamLgl"
  properties = "single-crit"
  packages = "GenSA"

  tuner = TunerBatch$new(
    id = "tuner",
    param_set = param_set,
    param_classes = param_classes,
    properties = "single-crit",
    packages = packages)

  expect_equal(tuner$param_set, param_set)
  expect_equal(tuner$param_classes, param_classes)
  expect_equal(tuner$properties, properties)
  expect_subset(packages, tuner$packages)

  expect_error({
    tuner$param_set = ps(p2 = p_lgl())
  },
  regexp = "$param_set is read-only",
  fixed = TRUE)

  expect_error({
    tuner$param_classes = "foo"
  },
  regexp = "$param_classes is read-only",
  fixed = TRUE)

  expect_error({
    tuner$properties = "foo"
  },
  regexp = "$properties is read-only",
  fixed = TRUE)

  expect_error({
    tuner$packages = "foo"
  },
  regexp = "$packages is read-only",
  fixed = TRUE)
})

test_that("internal single crit", {
  aggr = function(x) 99
  learner = lrn("classif.debug",
    iter = to_tune(upper = 1000L, internal = TRUE, aggr = aggr),
    x = to_tune(0.2, 0.3),
    validate = 0.3,
    early_stopping = TRUE
  )
  ti = tune(
    tuner = tnr("grid_search", batch_size = 2),
    learner = learner,
    task = tsk("iris"),
    resampling = rsmp("cv"),
    term_evals = 4
  )
  expect_equal(
    ti$archive$data$internal_tuned_values, replicate(list(list(iter = 99L)), n = 4L)
  )
  expect_equal(
    ti$result_learner_param_vals$iter, 99L
  )
})

test_that("internal single crit without benchmark_result", {
  aggr = function(x) 99
  learner = lrn("classif.debug",
    iter = to_tune(upper = 1000L, internal = TRUE, aggr = aggr),
    x = to_tune(0.2, 0.3),
    validate = 0.3,
    early_stopping = TRUE
  )
  ti = tune(
    tuner = tnr("grid_search", batch_size = 2),
    learner = learner,
    task = tsk("iris"),
    resampling = rsmp("cv"),
    term_evals = 4,
    store_benchmark_result = FALSE
  )
  expect_equal(
    ti$archive$data$internal_tuned_values, replicate(list(list(iter = 99L)), n = 4L)
  )
  expect_equal(
    ti$result_learner_param_vals$iter, 99L
  )
})


test_that("internal multi crit", {
  learner = lrn("classif.debug",
    iter = to_tune(upper = 1000L, internal = TRUE, aggr = function(x) as.integer(ceiling(mean(unlist(x))) + 2000L)),
    x = to_tune(0.2, 0.3),
    predict_type = "prob",
    validate = 0.3,
    early_stopping = TRUE
  )
  # this ensures we get a pareto front that contains all values
  m1 = msr("classif.acc")
  m2 = msr("classif.acc", id = "classif.acc2")
  m2$minimize = TRUE

  ti = tune(
    tuner = tnr("random_search", batch_size = 2),
    learner = learner,
    task = tsk("sonar"),
    resampling = rsmp("cv", folds = 2L),
    measures = list(m1, m2),
    term_evals = 20
  )

  expect_true(length(ti$result_learner_param_vals) == 20L)
  expect_true(all(map_int(ti$archive$data$internal_tuned_values, "iter") >= 2000L))
  expect_true(all(map_lgl(ti$result_learner_param_vals, function(x) x$iter >= 2000L)))
  expect_true(length(unique(map_int(ti$archive$data$internal_tuned_values, "iter"))) > 1L)

  expect_permutation(
    map_int(ti$result_learner_param_vals, "iter"),
    map_int(ti$archive$data$internal_tuned_values, "iter")
  )
})

test_that("proper error when primary search space is empty", {
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99)),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10)
  )

  tuner = tnr("random_search", batch_size = 1)
  expect_error(tuner$optimize(instance), "To only conduct")
})

test_that("internal tuning: branching", {
  skip_if_not_installed("mlr3pipelines")
  skip_if(packageVersion("mlr3pipelines") < "0.5.3")
  requireNamespace("mlr3pipelines")
  # this case is special, because not all internally tuned parameters are present in every iteration, only those that
  # are in the active branch are
  glrn = ppl("branch", graphs = list(
    lrn("classif.debug", id = "lrn1", iter = to_tune(upper = 500, internal = TRUE, aggr = function(x) 1L), early_stopping = TRUE),
    lrn("classif.debug", id = "lrn2", iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 2L), early_stopping = TRUE)
  ))

  learner = as_learner(glrn)
  set_validate(learner, 0.2, ids = c("lrn1", "lrn2"))

  task = tsk("iris")
  learner$param_set$set_values(
    branch.selection = to_tune()
  )

  instance = ti(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10)
  )

  tuner = tnr("grid_search")
  tuner$optimize(instance)

  expect_equal(
    instance$archive$data[list(1), "internal_tuned_values", on = "branch.selection"][[1L]][[1L]]$lrn1.iter,
    1L
  )
  expect_equal(
    instance$archive$data[list(2), "internal_tuned_values", on = "branch.selection"][[1L]][[1L]]$lrn2.iter,
    2L
  )
})

test_that("internal tuning: error is thrown on incorrect configuration", {
  expect_error(tune(
    tuner = tnr("random_search"),
    learner = lrn("classif.debug", iter = to_tune(upper = 1000, internal = TRUE)),
    task = tsk("iris"),
    resampling = rsmp("holdout")
  ), "early_stopping")
})

test_that("internal tuning: error message when primary search space is empty", {
  expect_error(tune(
    tuner = tnr("random_search"),
    learner = lrn("classif.debug", iter = to_tune(upper = 1000, internal = TRUE), early_stopping = TRUE, validate = 0.2),
    task = tsk("iris"),
    resampling = rsmp("holdout")
  ), "tnr('internal')", fixed = TRUE)
})
