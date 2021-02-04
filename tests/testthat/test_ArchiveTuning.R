test_that("ArchiveTuning access methods work", {
  instance = TuningInstanceSingleCrit$new(task = tsk("iris"), learner = lrn("classif.rpart"), 
    resampling = rsmp("holdout"), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
    terminator = trm("evals", n_evals = 4))

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  map(instance$archive$data$uhash, function(uhash) {
    expect_learner(instance$archive$learner(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_learner(instance$archive$learner(i))
  })

  map(instance$archive$data$uhash, function(uhash) {
    expect_resample_result(instance$archive$resample_result(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })

  expect_data_table(instance$archive$extended_archive, nrow = 4)
  expect_named(instance$archive$extended_archive, c(names(instance$archive$data), "nr", "resample_result"))

  instance = TuningInstanceSingleCrit$new(task = tsk("iris"), learner = lrn("classif.rpart"), 
    resampling = rsmp("cv", folds = 2), measure = msr("classif.ce"), search_space = TEST_MAKE_PS1(n_dim = 1), 
    terminator = trm("evals", n_evals = 4))

  tuner = tnr("random_search", batch_size = 2)
  tuner$optimize(instance)

  map(instance$archive$data$uhash, function(uhash) {
    expect_learner(instance$archive$learner(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_learner(instance$archive$learner(i))
  })

  map(instance$archive$data$uhash, function(uhash) {
    expect_resample_result(instance$archive$resample_result(uhash = uhash))
  })

  map(seq(nrow(instance$archive$data)), function(i) {
    expect_resample_result(instance$archive$resample_result(i))
  })

  expect_data_table(instance$archive$extended_archive, nrow = 4)
  expect_named(instance$archive$extended_archive, c(names(instance$archive$data), "nr", "resample_result"))
})
