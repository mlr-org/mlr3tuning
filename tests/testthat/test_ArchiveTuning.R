test_that("ArchiveTuning access methods work", {
  instance = TuningInstanceSingleCrit$new(task = tsk("iris"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
    terminator = trm("evals", n_evals = 4))

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  # learner
  map(instance$archive$data$uhash, function(uhash) {
    expect_learner(instance$archive$learner(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_learner(instance$archive$learner(i))
  })

  # learner param values
  map(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learner_param_vals(uhash = uhash))
    expect_named(instance$archive$learner_param_vals(uhash = uhash), c("xval" ,"cp"))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_list(instance$archive$learner_param_vals(i))
    expect_named(instance$archive$learner_param_vals(i), c("xval" ,"cp"))
  })

  # learners
  map(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learners(uhash = uhash))
    expect_learner(instance$archive$learners(uhash = uhash)[[1]])
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_list(instance$archive$learners(i))
    expect_learner(instance$archive$learners(i)[[1]])
  })

  # predictions
  map(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$predictions(uhash = uhash))
    expect_prediction(instance$archive$predictions(uhash = uhash)[[1]])
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_list(instance$archive$predictions(i))
    expect_prediction(instance$archive$predictions(i)[[1]])
  })

  # resample result
  map(instance$archive$data$uhash, function(uhash) {
    expect_resample_result(instance$archive$resample_result(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })

    # learner
  map(instance$archive$data$uhash, function(uhash) {
    expect_learner(instance$archive$learner(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_learner(instance$archive$learner(i))
  })

  # learner param values
  map(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learner_param_vals(uhash = uhash))
    expect_named(instance$archive$learner_param_vals(uhash = uhash), c("xval" ,"cp"))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_list(instance$archive$learner_param_vals(i))
    expect_named(instance$archive$learner_param_vals(i), c("xval" ,"cp"))
  })

  # learners
  map(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$learners(uhash = uhash))
    expect_learner(instance$archive$learners(uhash = uhash)[[1]])
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_list(instance$archive$learners(i))
    expect_learner(instance$archive$learners(i)[[1]])
  })

  # predictions
  map(instance$archive$data$uhash, function(uhash) {
    expect_list(instance$archive$predictions(uhash = uhash))
    expect_prediction(instance$archive$predictions(uhash = uhash)[[1]])
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_list(instance$archive$predictions(i))
    expect_prediction(instance$archive$predictions(i)[[1]])
  })

  # resample result
  map(instance$archive$data$uhash, function(uhash) {
    expect_resample_result(instance$archive$resample_result(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })
})

test_that("ArchiveTuning as.data.table function works", {
  instance = TuningInstanceSingleCrit$new(task = tsk("pima"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
    terminator = trm("evals", n_evals = 4))

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  # default
  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 7)
  expect_named(tab, c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result"))

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, nrows = 4, ncols = 8)
  expect_named(tab, c("cp", "classif.ce", "classif.acc", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result"))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, nrows = 4, ncols = 9)
  expect_named(tab, c("cp", "classif.ce", "classif.acc", "classif.mcc", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result"))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp")
  expect_data_table(tab, nrows = 4, ncols = 7)
  expect_named(tab, c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "batch_nr", "uhash", "resample_result"))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp", "uhash"))
  expect_data_table(tab, nrows = 4, ncols = 6)
  expect_named(tab, c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "batch_nr", "resample_result"))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, nrows = 4, ncols = 8)
  expect_named(tab, c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "uhash", "resample_result"))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, nrows = 4, ncols = 7)
  expect_named(tab, c("cp", "classif.ce" ,"runtime_learners", "timestamp", "batch_nr", "x_domain", "resample_result"))

  # without benchmark result
  instance = TuningInstanceSingleCrit$new(task = tsk("pima"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
    terminator = trm("evals", n_evals = 4), store_benchmark_result = FALSE)

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 6)
  expect_named(tab, c("cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr"))

  # empty archive
  instance = TuningInstanceSingleCrit$new(task = tsk("pima"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
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

  instance = tune(method = "random_search", task = tsk("pima"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = search_space, 
    term_evals = 4, batch_size = 2)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 9)
  expect_named(tab, c("x1", "x2", "classif.ce", "x_domain_minsplit", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result"))

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

  instance = tune(method = "random_search", task = tsk("pima"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = search_space, 
    term_evals = 4, batch_size = 2)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 9)
  expect_named(tab, c("x1", "x2", "classif.ce", "x_domain_minsplit", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result"))

  # row order
  instance = TuningInstanceSingleCrit$new(task = tsk("pima"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
    terminator = trm("evals", n_evals = 10))

  tuner = tnr("random_search", batch_size = 1)
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_equal(tab$batch_nr, 1:10)
})
