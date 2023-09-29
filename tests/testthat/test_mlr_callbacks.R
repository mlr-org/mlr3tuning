test_that("early stopping callback works", {
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint
  library(mlr3pipelines) # nolint

  instance = tune(
    tuner = tnr("random_search", batch_size = 1),
    task = tsk("pima"),
    learner = lrn("classif.xgboost", eta = to_tune(1e-04, 1e-1, logscale = TRUE), early_stopping_rounds = 20, nrounds = 1000, early_stopping_set = "test"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = clbk("mlr3tuning.early_stopping")
  )

  expect_numeric(instance$archive$best()$max_nrounds)
  expect_equal(instance$archive$best()$max_nrounds, instance$result_learner_param_vals$nrounds)
  expect_equal(instance$result_learner_param_vals$early_stopping_set, "none")
  expect_null(instance$result_learner_param_vals$early_stopping_rounds)

  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 1),
    learner = lrn("classif.xgboost", eta = to_tune(1e-04, 1e-1, logscale = TRUE), early_stopping_rounds = 20, nrounds = 1000, early_stopping_set = "test"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 2,
    callbacks = clbk("mlr3tuning.early_stopping")
  )
  at$train(tsk("pima"))

  expect_numeric(instance$archive$best()$max_nrounds)
  expect_equal(at$tuning_instance$archive$best()$max_nrounds, at$learner$param_set$values$nrounds)
  expect_equal(at$learner$param_set$values$early_stopping_set, "none")
  expect_null(at$learner$param_set$values$early_stopping_rounds)

  expect_error(tune(
    tuner = tnr("random_search",  batch_size = 5),
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 10,
    callbacks = clbk("mlr3tuning.early_stopping")
  ), "incompatible")
})

test_that("backup callback works", {
  file = tempfile(fileext = ".rds")

  instance = tune(
    tuner =  tnr("random_search", batch_size = 2),
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 4,
    callbacks = clbk("mlr3tuning.backup", path = file)
  )

  expect_file_exists(file)
  expect_benchmark_result(readRDS(file))
})

test_that("backup callback works with standalone tuner", {
  file = tempfile(fileext = ".rds")

  instance = tune(
    tuner =  tnr("grid_search", batch_size = 2),
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 4,
    callbacks = clbk("mlr3tuning.backup", path = file)
  )

  expect_file_exists(file)
  expect_benchmark_result(readRDS(file))
})

# rush -------------------------------------------------------------------------

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
