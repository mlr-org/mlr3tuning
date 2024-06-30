test_that("TunerAsyncRandomSearch works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128),
    cp        = to_tune(1e-04, 1e-1))

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  tuner = tnr("async_random_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)

  expect_data_table(instance$archive$data, min.rows = 20)
  expect_rush_reset(instance$rush, type = "terminate")
})

test_that("internal tuning: single-crit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, x = to_tune(0.2, 0.3),
    iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99))

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = TRUE
  )

  tuner = tnr("async_random_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)
  expect_equal(
    instance$archive$data$internal_tuned_values,
    replicate(list(list(iter = 99)), n = length(instance$archive$data$internal_tuned_values))
  )
  expect_false(instance$result_learner_param_vals$early_stopping)
  expect_equal(instance$result_learner_param_vals$iter, 99)
})

test_that("internal tuning: multi-crit", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

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
    tuner = tnr("async_random_search"),
    learner = learner,
    task = tsk("sonar"),
    resampling = rsmp("cv", folds = 2L),
    measures = list(m1, m2),
    term_evals = 20
  )

  expect_true(length(ti$result_learner_param_vals) >= 20L)
  expect_true(all(map_int(ti$archive$data$internal_tuned_values, "iter") >= 2000L))
  expect_true(all(map_lgl(ti$result_learner_param_vals, function(x) x$iter >= 2000L)))
  expect_true(length(unique(map_int(ti$archive$data$internal_tuned_values, "iter"))) > 1L)

  expect_permutation(
    map_int(ti$result_learner_param_vals, "iter"),
    map_int(ti$archive$data$internal_tuned_values, "iter")
  )
})


test_that("internal tuning: error is thrown on incorrect configuration", {
  expect_error(tune(
    tuner = tnr("async_random_search"),
    learner = lrn("classif.debug", iter = to_tune(upper = 1000, internal = TRUE)),
    task = tsk("iris"),
    resampling = rsmp("holdout")
  ), "early_stopping")
})

test_that("internal tuning: error message when primary search space is empty", {
  skip_on_cran()

  expect_error(tune(
    tuner = tnr("async_random_search"),
    learner = lrn("classif.debug", iter = to_tune(upper = 1000, internal = TRUE), early_stopping = TRUE, validate = 0.2),
    task = tsk("iris"),
    resampling = rsmp("holdout")
  ), "tnr('internal')", fixed = TRUE)
})
