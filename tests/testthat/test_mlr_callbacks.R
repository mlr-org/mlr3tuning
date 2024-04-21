test_that("early stopping callback works", {
  skip_on_cran()
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

# async ------------------------------------------------------------------------

test_that("async hotstart callback works", {
  options(bbotk_local = TRUE)
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  lgr::get_logger("mlr3")$set_threshold("debug")
  lgr::get_logger("bbotk")$set_threshold("debug")

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.debug", x = to_tune(), iter = to_tune(1, 100)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE,
    callbacks = clbk("mlr3tuning.async_hotstart")
  )

  design = data.table(
    x = 1,
    iter = c(1, 2, 3)
  )

  tuner = tnr("async_design_points", design = design)
  tuner$optimize(instance)


  ids = map(extract_benchmark_result_learners(instance$archive$benchmark_result), function(l) l$model$id)
  expect_equal(length(unique(ids)), 5)
  expect_equal(unique(instance$archive$data$iter), c(1, 26, 51, 76, 101))
  expect_null(instance$archive$data$resample_result)

  tuner = tnr("async_random_search")
  expect_data_table(tuner$optimize(instance), nrows = 1)
})


# rush -------------------------------------------------------------------------

test_that("rush early stopping callback works", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not_installed("mlr3learners")
  skip_if_not_installed("xgboost")
  library(mlr3learners) # nolint
  library(mlr3pipelines) # nolint

  rush = rsh()
  instance = TuningInstanceAsyncSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.xgboost", eta = to_tune(1e-04, 1e-1, logscale = TRUE), early_stopping_rounds = 2, nrounds = 10, early_stopping_set = "test"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    allow_hotstart = TRUE,
    callbacks = clbk("mlr3tuning.rush_early_stopping"),
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE, lgr_thresholds = c(rush = "debug", bbotk = "debug", mlr3 = "debug"))
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  expect_numeric(instance$archive$best()$max_nrounds)
  expect_equal(instance$archive$best()$max_nrounds, instance$result_learner_param_vals$nrounds)
  expect_equal(instance$result_learner_param_vals$early_stopping_set, "none")
  expect_null(instance$result_learner_param_vals$early_stopping_rounds)

  rush$reset()
})

test_that("rush measures callback works", {
  skip_on_cran()
  skip_on_ci()

  rush = rsh()
  instance = TuningInstanceAsyncSingleCrit$new(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1), predict_sets = c("test", "holdout")),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    allow_hotstart = TRUE,
    callbacks = clbk("mlr3tuning.rush_measures", measures = list(msr("classif.ce", predict_sets = "holdout", id = "classif.ce_holdout"))),
    rush = rush
  )
  future::plan("cluster", workers = 1L)
  instance$start_workers(await_workers = TRUE, lgr_thresholds = c(rush = "debug", bbotk = "debug", mlr3 = "debug"))
  pids = rush$worker_info$pid
  on.exit({clean_on_exit(pids)}, add = TRUE)

  tuner = tnr("random_search")
  tuner$optimize(instance)

  expect_numeric(instance$archive$data$classif.ce_holdout)

  rush$reset()
})

test_that("rush mlflow callback works", {
  skip_on_cran()
  skip_on_ci()
  skip_if(TRUE)

  flush_redis()
  rush::rush_plan(n_workers = 4)

  learner = lrn("classif.rpart",
    minsplit  = to_tune(2, 128),
    cp        = to_tune(1e-04, 1e-1))

  instance = TuningInstanceAsyncSingleCrit$new(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE,
    callbacks = clbk("mlr3tuning.rush_mlflow", tracking_uri = "http://localhost:8080")
  )

  optimizer = tnr("random_search_v2")
  optimizer$optimize(instance)
})
