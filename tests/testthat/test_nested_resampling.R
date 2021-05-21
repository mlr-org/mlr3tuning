test_that("extract_inner_tuning_results function works", {
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  search_space = TEST_MAKE_PS1(n_dim = 1)
  ms = msr("classif.ce")
  tuner = tnr("grid_search", resolution = 3)

  # cv
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  irr = extract_inner_tuning_results(rr)
  expect_data_table(irr, nrows = 2) 
  expect_named(irr, c("cp", "learner_param_vals", "x_domain", "classif.ce", "iteration"))

  # repeated cv
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  irr = extract_inner_tuning_results(rr)
  expect_data_table(irr, nrows = 6) 
  expect_named(irr, c("cp", "learner_param_vals", "x_domain", "classif.ce", "iteration"))

  # cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_results(bmr)
  expect_data_table(ibmr, nrows = 4) 
  expect_named(ibmr, c("cp", "learner_param_vals", "x_domain", "classif.ce", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

   # repeated cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_results(bmr)
  expect_data_table(ibmr, nrows = 12) 
  expect_named(ibmr, c("cp", "learner_param_vals", "x_domain", "classif.ce", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))
})

test_that("extract_inner_tuning_archives function works", {
  te = trm("evals", n_evals = 4)
  task = tsk("iris")
  search_space = TEST_MAKE_PS1(n_dim = 1)
  ms = msr("classif.ce")
  tuner = tnr("grid_search", resolution = 3)

  # cv
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  irr = extract_inner_tuning_archives(rr)
  expect_data_table(irr, nrows = 6) 
  expect_named(irr, c("cp", "classif.ce", "uhash", "x_domain", "timestamp", "batch_nr", "iteration"))

  # repeated cv
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  irr = extract_inner_tuning_archives(rr)
  expect_data_table(irr, nrows = 18) 
  expect_named(irr, c("cp", "classif.ce", "uhash", "x_domain", "timestamp", "batch_nr", "iteration"))

  # cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 12) 
  expect_named(ibmr, c("cp", "classif.ce", "uhash", "x_domain", "timestamp", "batch_nr", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

   # repeated cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 36) 
  expect_named(ibmr, c("cp", "classif.ce", "uhash", "x_domain", "timestamp", "batch_nr", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))
})
