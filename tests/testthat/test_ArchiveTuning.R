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
  expect_named(tab, c("cp", "classif.ce", "runtime", "timestamp", "batch_nr", "x_domain_cp", "resample_result"))

  # extra measure
  tab = as.data.table(instance$archive, measures = msr("classif.acc"))
  expect_data_table(tab, nrows = 4, ncols = 8)
  expect_named(tab, c("cp", "classif.ce", "classif.acc", "runtime", "timestamp", "batch_nr", "x_domain_cp", "resample_result"))

  # extra measures
  tab = as.data.table(instance$archive, measures = msrs(c("classif.acc", "classif.mcc")))
  expect_data_table(tab, nrows = 4, ncols = 9)
  expect_named(tab, c("cp", "classif.ce", "classif.acc", "classif.mcc" ,"runtime", "timestamp", "batch_nr", "x_domain_cp", "resample_result"))

  # exclude column
  tab = as.data.table(instance$archive, exclude_columns = "timestamp")
  expect_data_table(tab, nrows = 4, ncols = 7)
  expect_named(tab, c("cp", "classif.ce" ,"runtime", "batch_nr", "uhash", "x_domain_cp", "resample_result"))

  # exclude columns
  tab = as.data.table(instance$archive, exclude_columns = c("timestamp", "uhash"))
  expect_data_table(tab, nrows = 4, ncols = 6)
  expect_named(tab, c("cp", "classif.ce" ,"runtime", "batch_nr", "x_domain_cp", "resample_result"))

  # no exclude
  tab = as.data.table(instance$archive, exclude_columns = NULL)
  expect_data_table(tab, nrows = 4, ncols = 8)
  expect_named(tab, c("cp", "classif.ce" ,"runtime", "timestamp", "batch_nr", "uhash", "x_domain_cp", "resample_result"))

  # no unnest
  tab = as.data.table(instance$archive, unnest = NULL)
  expect_data_table(tab, nrows = 4, ncols = 7)
  expect_named(tab, c("cp", "classif.ce" ,"runtime", "timestamp", "batch_nr", "x_domain", "resample_result"))

  # without benchmark result
  instance = TuningInstanceSingleCrit$new(task = tsk("pima"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
    terminator = trm("evals", n_evals = 4), store_benchmark_result = FALSE)

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  tab = as.data.table(instance$archive)
  expect_data_table(tab, nrows = 4, ncols = 6)
  expect_named(tab, c("cp", "classif.ce", "runtime", "timestamp", "batch_nr", "x_domain_cp"))

  # empty archive
  instance = TuningInstanceSingleCrit$new(task = tsk("pima"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
    terminator = trm("evals", n_evals = 4))

  expect_data_table(as.data.table(instance$archive), nrows = 0, ncols = 0)
})
