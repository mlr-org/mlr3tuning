test_that("tuning with rush works", {
  skip_on_cran()
  skip_on_ci()

  config = start_flush_redis()
  rush = Rush$new("test", config)

  learner = lrn("classif.rpart",
    cp = to_tune(0.01, 0.1)
  )

  instance = ti(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    store_models = FALSE,
    store_benchmark_result = FALSE,
    rush = rush,
    freeze_archive = FALSE
  )

  future::plan("multisession", workers = 2)
  instance$start_workers()
  rush$await_workers(2)

  expect_equal(rush$n_workers, 2)

  tuner = tnr("random_search")

  expect_data_table(tuner$optimize(instance), nrows = 1)

  expect_data_table(as.data.table(instance$archive))
})

test_that("freeze archive works", {
  skip_on_cran()
  skip_on_ci()

  config = start_flush_redis()
  rush = Rush$new("test", config)

  learner = lrn("classif.rpart",
    cp = to_tune(0.01, 0.1)
  )

  instance = ti(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    store_models = FALSE,
    store_benchmark_result = FALSE,
    rush = rush,
    freeze_archive = TRUE
  )

  future::plan("multisession", workers = 2)
  instance$start_workers()
  rush$await_workers(2)

  expect_equal(rush$n_workers, 2)

  tuner = tnr("random_search")

  tuner$optimize(instance)

  expect_null(instance$archive$rush)
  expect_data_table(instance$archive$data, min.rows = 10L)
})

test_that("rush early stopping callback works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint
  library(mlr3pipelines) # nolint

  # check stages in objective
  callback = clbk("mlr3tuning.rush_early_stopping")
  callback$state$store_models = TRUE

  objective = ObjectiveRushTuning$new(
    task = tsk("pima"),
    learner = lrn("classif.xgboost", eta = to_tune(1e-04, 1e-1, logscale = TRUE), early_stopping_rounds = 20, nrounds = 1000, early_stopping_set = "test"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    callbacks = callback,
    store_models = TRUE
  )

  expect_number(objective$eval(list(eta = 1e-04))$max_nrounds)

  # test all stages
  config = start_flush_redis()
  rush = Rush$new("test", config)

  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.xgboost", eta = to_tune(1e-04, 1e-1, logscale = TRUE), early_stopping_rounds = 20, nrounds = 1000, early_stopping_set = "test"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2),
    callbacks = clbk("mlr3tuning.rush_early_stopping"),
    store_models = TRUE,
    rush = rush
  )

  tuner = tnr("random_search")

  future::plan("multisession", workers = 2)
  instance$start_workers()
  rush$await_workers(2)

  tuner$optimize(instance)

  expect_numeric(instance$archive$best()$max_nrounds)
  expect_equal(instance$archive$best()$max_nrounds, instance$result_learner_param_vals$nrounds)
  expect_equal(instance$result_learner_param_vals$early_stopping_set, "none")
  expect_null(instance$result_learner_param_vals$early_stopping_rounds)
})
