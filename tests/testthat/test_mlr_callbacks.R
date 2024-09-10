# batch backup callback --------------------------------------------------------

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

# async measure callback ------------------------------------------------------

test_that("async measures callback works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1), predict_sets = "test"),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 3),
    callbacks = clbk("mlr3tuning.async_measures", measures = list(msr("classif.ce", predict_sets = "test", id = "classif.ce_holdout"))))

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_numeric(instance$archive$data$classif.ce_holdout)

  expect_rush_reset(instance$rush, type = "kill")
})

# async mlflow callback --------------------------------------------------------

# test_that("rush mlflow callback works", {
#   # mlflow server must be running
#   skip_on_ci()
#   skip_if_not_installed("rush")
#   flush_redis()

#   learner = lrn("classif.rpart",
#     minsplit  = to_tune(2, 128),
#     cp        = to_tune(1e-04, 1e-1))

#   rush::rush_plan(n_workers = 2)
#   instance = ti_async(
#     task = tsk("pima"),
#     learner = learner,
#     resampling = rsmp("cv", folds = 3),
#     measures = msr("classif.ce"),
#     terminator = trm("evals", n_evals = 20),
#     store_benchmark_result = FALSE,
#     callbacks = clbk("mlr3tuning.async_mlflow", tracking_uri = "http://localhost:8080")
#   )

#   optimizer = tnr("async_random_search")
#   optimizer$optimize(instance)
# })

# async default configuration callback -----------------------------------------

test_that("default configuration callback works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    callbacks = clbk("mlr3tuning.async_default_configuration")
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], 0.01)
})

test_that("default configuration callback works with logscale", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    callbacks = clbk("mlr3tuning.async_default_configuration")
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], log(0.01))
})

test_that("default configuration callback errors with trafo", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune(p_dbl(-10, 0, trafo = function(x) 10^x))),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    callbacks = clbk("mlr3tuning.async_default_configuration")
  )

  tuner = tnr("async_random_search")
  expect_error(tuner$optimize(instance), "Cannot evaluate default hyperparameter values")
})

test_that("default configuration callback works without transformation and with logscale", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart",
    cp = to_tune(1e-3, 1, logscale = TRUE),
    minbucket = to_tune(1, 20))

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    callbacks = clbk("mlr3tuning.async_default_configuration")
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], log(0.01))
  expect_equal(instance$archive$data$x_domain[[1]]$minbucket, 7)
  expect_equal(instance$archive$data$minbucket[[1]], 7)
})

test_that("default configuration callback errors without transformation and with logscale and trafo", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart",
    cp = to_tune(1e-3, 1, logscale = TRUE),
    minbucket = to_tune(1, 20),
    minsplit = to_tune(p_int(0, 3, trafo = function(x) 2^x)))

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    callbacks = clbk("mlr3tuning.async_default_configuration")
  )

  tuner = tnr("async_random_search")
  expect_error(tuner$optimize(instance), "Cannot evaluate default hyperparameter values")
})

test_that("default configuration callback errors with extra trafo", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart")
  search_space = ps(
    cp = p_dbl(1e-3, 1, logscale = TRUE),
    minbucket = p_int(1, 20),
    minsplit = p_int(1, 20),
    .extra_trafo = function(x, param_set) {
      x$minsplit = 3
      x
    }
  )

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    search_space = search_space,
    callbacks = clbk("mlr3tuning.async_default_configuration")
  )

  tuner = tnr("async_random_search")
  expect_error(tuner$optimize(instance), "Cannot evaluate default hyperparameter values")
})

test_that("default configuration callback errors with old parameter set api", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  learner = lrn("classif.rpart")
  search_space = ps(
    cp = p_dbl(lower = -10, upper = 0, trafo = function(x) 10^x)
  )

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    search_space = search_space,
    callbacks = clbk("mlr3tuning.async_default_configuration")
  )

  tuner = tnr("async_random_search")
  expect_error(tuner$optimize(instance), "Cannot evaluate default hyperparameter values")
})

# batch default configuration callback -----------------------------------------

test_that("batch default configuration callback  works", {
  learner = lrn("classif.rpart", cp = to_tune(1e-3, 1))

  instance = tune(
    tuner = tnr("random_search"),
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = clbk("mlr3tuning.default_configuration")
  )

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], 0.01)
})

test_that("batch default configuration callback  works with logscale", {
  learner = lrn("classif.rpart", cp = to_tune(1e-3, 1, logscale = TRUE))

  instance = tune(
    tuner = tnr("random_search"),
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = clbk("mlr3tuning.default_configuration")
  )

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], log(0.01))
})

test_that("batch default configuration callback  errors with trafo", {
  learner = lrn("classif.rpart", cp = to_tune(p_dbl(-10, 0, trafo = function(x) 10^x)))

  expect_error(tune(
    tuner = tnr("random_search"),
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = clbk("mlr3tuning.default_configuration")
  ), "Cannot evaluate default hyperparameter values")
})

test_that("batch default configuration callback  works without transformation and with logscale", {
  learner = lrn("classif.rpart",
    cp = to_tune(1e-3, 1, logscale = TRUE),
    minbucket = to_tune(1, 20))

  instance = tune(
    tuner = tnr("random_search"),
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = clbk("mlr3tuning.default_configuration")
  )

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], log(0.01))
  expect_equal(instance$archive$data$x_domain[[1]]$minbucket, 7)
  expect_equal(instance$archive$data$minbucket[[1]], 7)
})

test_that("batch default configuration callback  errors without transformation and with logscale and trafo", {
  learner = lrn("classif.rpart",
    cp = to_tune(1e-3, 1, logscale = TRUE),
    minbucket = to_tune(1, 20),
    minsplit = to_tune(p_int(0, 3, trafo = function(x) 2^x)))

  expect_error(tune(
    tuner = tnr("random_search"),
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    callbacks = clbk("mlr3tuning.default_configuration")
  ), "Cannot evaluate default hyperparameter values")
})

test_that("batch default configuration callback  errors with extra trafo", {
  learner = lrn("classif.rpart")
  search_space = ps(
    cp = p_dbl(1e-3, 1, logscale = TRUE),
    minbucket = p_int(1, 20),
    minsplit = p_int(1, 20),
    .extra_trafo = function(x, param_set) {
      x$minsplit = 3
      x
    }
  )

  expect_error(tune(
    tuner = tnr("random_search"),
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    search_space = search_space,
    term_evals = 2,
    callbacks = clbk("mlr3tuning.default_configuration")
  ), "Cannot evaluate default hyperparameter values")
})

test_that("batch default configuration callback  errors with old parameter set api", {
  learner = lrn("classif.rpart")
  search_space = ps(
    cp = p_dbl(lower = -10, upper = 0, trafo = function(x) 10^x)
  )

  expect_error(tune(
    tuner = tnr("random_search"),
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    search_space = search_space,
    term_evals = 2,
    callbacks = clbk("mlr3tuning.default_configuration")
  ), "Cannot evaluate default hyperparameter values")
})

# async save logs callback -----------------------------------------------------

test_that("async save logs callback works", {
  skip_on_cran()
  skip_if_not_installed("rush")
  flush_redis()

  rush::rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.debug", message_train = 1, x = to_tune()),
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 5),
    callbacks = clbk("mlr3tuning.async_save_logs")
  )

  tuner = tnr("async_random_search")
  tuner$optimize(instance)

  expect_list(instance$archive$data$log)
  expect_data_table(instance$archive$data$log[[1]][[1]])
})


