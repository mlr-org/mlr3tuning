test_that("ArchiveTuning access methods work", {
  instance = ti(
    task = tsk("iris"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    search_space = TEST_MAKE_PS1(n_dim = 1),
    terminator = trm("evals", n_evals = 4))

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  # learner
  walk(instance$archive$data$uhash, function(uhash) {
    expect_learner(instance$archive$learner(uhash = uhash))
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_learner(instance$archive$learner(i))
  })

  # learner param values
  walk(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learner_param_vals(uhash = uhash))
    expect_named(instance$archive$learner_param_vals(uhash = uhash), c("xval" ,"cp"), ignore.order = TRUE)
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_list(instance$archive$learner_param_vals(i))
    expect_named(instance$archive$learner_param_vals(i), c("xval" ,"cp"), ignore.order = TRUE)
  })

  # learners
  walk(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learners(uhash = uhash))
    expect_learner(instance$archive$learners(uhash = uhash)[[1]])
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_list(instance$archive$learners(i))
    expect_learner(instance$archive$learners(i)[[1]])
  })

  # predictions
  walk(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$predictions(uhash = uhash))
    expect_prediction(instance$archive$predictions(uhash = uhash)[[1]])
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_list(instance$archive$predictions(i))
    expect_prediction(instance$archive$predictions(i)[[1]])
  })

  # resample result
  walk(instance$archive$data$uhash, function(uhash) {
    expect_resample_result(instance$archive$resample_result(uhash = uhash))
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })

    # learner
  walk(instance$archive$data$uhash, function(uhash) {
    expect_learner(instance$archive$learner(uhash = uhash))
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_learner(instance$archive$learner(i))
  })

  # learner param values
  walk(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learner_param_vals(uhash = uhash))
    expect_named(instance$archive$learner_param_vals(uhash = uhash), c("xval" ,"cp"), ignore.order = TRUE)
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_list(instance$archive$learner_param_vals(i))
    expect_named(instance$archive$learner_param_vals(i), c("xval" ,"cp"), ignore.order = TRUE)
  })

  # learners
  walk(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learners(uhash = uhash))
    expect_learner(instance$archive$learners(uhash = uhash)[[1]])
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_list(instance$archive$learners(i))
    expect_learner(instance$archive$learners(i)[[1]])
  })

  # predictions
  walk(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$predictions(uhash = uhash))
    expect_prediction(instance$archive$predictions(uhash = uhash)[[1]])
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_list(instance$archive$predictions(i))
    expect_prediction(instance$archive$predictions(i)[[1]])
  })

  # resample result
  walk(instance$archive$data$uhash, function(uhash) {
    expect_resample_result(instance$archive$resample_result(uhash = uhash))
  })

  walk(seq_row(instance$archive$data), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })
})

test_that("ArchiveTuning as.data.table function works", {
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    search_space = TEST_MAKE_PS1(n_dim = 1),
    terminator = trm("evals", n_evals = 4))

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  # default
  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4)
  expect_names(names(tab), permutation.of =  c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "errors", "warnings"))

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, nrows = 4)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "classif.acc", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "errors", "warnings"))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, nrows = 4)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "classif.acc", "classif.mcc", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "errors", "warnings"))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp")
  expect_data_table(tab, nrows = 4)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "batch_nr", "uhash", "resample_result", "errors", "warnings"))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp", "uhash"))
  expect_data_table(tab, nrows = 4)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "batch_nr", "resample_result", "errors", "warnings"))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, nrows = 4)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "uhash", "resample_result", "errors", "warnings"))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, nrows = 4)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce" ,"runtime_learners", "timestamp", "batch_nr", "x_domain", "resample_result", "errors", "warnings"))

  # without benchmark result
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    search_space = TEST_MAKE_PS1(n_dim = 1),
    terminator = trm("evals", n_evals = 4),
    store_benchmark_result = FALSE)

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 8)
  expect_names(names(tab), permutation.of = c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "errors", "warnings"))

  # empty archive
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    search_space = TEST_MAKE_PS1(n_dim = 1),
    terminator = trm("evals", n_evals = 4))

  expect_data_table(as.data.table(instance$archive), nrows = 0, ncols = 0)

  # new ids in x_domain
  search_space = ps(
    x1 = p_int(1, 12),
    x2 = p_dbl(0.01, 0.1),
    .extra_trafo = function(x, param_set) {
      x$minsplit = x$x1
      x$cp = x$x2
      x$x1 = NULL
      x$x2 = NULL
      x
    }
  )

  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 4),
    search_space = search_space)

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4)
  expect_names(names(tab), permutation.of = c("x1", "x2", "classif.ce", "x_domain_minsplit", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "errors", "warnings"))

  # new ids in x_domain switch
  search_space = ps(
    x1 = p_int(1, 12),
    x2 = p_dbl(0.01, 0.1),
    .extra_trafo = function(x, param_set) {

      if (x$x1 > 3) x$minsplit = x$x1
      x$cp = x$x2
      x$x1 = NULL
      x$x2 = NULL
      x
    }
  )

 instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 100),
    search_space = search_space)

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 100)
  expect_names(names(tab), permutation.of = c("x1", "x2", "classif.ce", "x_domain_minsplit", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "errors", "warnings"))

  # row order
 instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart"),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    search_space = search_space)

  tuner = tnr("random_search", batch_size = 1)
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_equal(tab$batch_nr, 1:10)
})

test_that("internally tuned values are included when converting archive to data.table", {
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99),
      x = to_tune(0.1, 0.3)),
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2)
  )

  tuner = tnr("random_search", batch_size = 1)
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_equal(tab$internal_tuned_values, replicate(list(list(iter = 99L)), n = 2L))
})
