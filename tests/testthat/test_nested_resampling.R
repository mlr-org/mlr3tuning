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
  expect_named(ibmr, c("cp", "classif.ce", "learner_param_vals", "x_domain", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

   # repeated cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_results(bmr)
  expect_data_table(ibmr, nrows = 12)
  expect_named(ibmr, c("cp", "classif.ce", "learner_param_vals", "x_domain", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

  # different hyperparameters
  at_1 = AutoTuner$new(lrn("classif.rpart", cp = to_tune(0.001, 0.1)), rsmp("holdout"), ms, te, tuner = tuner)
  at_2 = AutoTuner$new(lrn("classif.debug", x = to_tune()), rsmp("holdout"), ms, te, tuner = tuner)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_results(bmr)
  expect_data_table(ibmr, nrows = 4)
  expect_named(ibmr, c("cp", "x", "classif.ce", "learner_param_vals", "x_domain", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

  # error handling
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = FALSE)

  expect_error(extract_inner_tuning_results(rr),
    regex = "Set `store_models = TRUE` in `resample()` or `benchmark()`",
    fixed = TRUE)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space,
    store_tuning_instance = FALSE, store_benchmark_result = FALSE)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  expect_error(extract_inner_tuning_results(rr),
    regex = "Set `store_tuning_instance = TRUE` in <AutoTuner:classif.rpart.tuned>.",
    fixed = TRUE)

  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = FALSE)

  expect_error(extract_inner_tuning_results(bmr), regex = "store_models")

  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space,
    store_tuning_instance = FALSE, store_benchmark_result = FALSE)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  expect_error(extract_inner_tuning_results(bmr), regex = "store_tuning_instance")
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
  expect_named(irr, c("cp", "classif.ce", "runtime" ,"uhash", "x_domain", "timestamp", "batch_nr", "iteration"))

  # repeated cv
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  irr = extract_inner_tuning_archives(rr)
  expect_data_table(irr, nrows = 18)
  expect_named(irr, c("cp", "classif.ce", "runtime", "uhash", "x_domain", "timestamp", "batch_nr", "iteration"))

  # cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 12)
  expect_named(ibmr, c("cp", "classif.ce", "runtime", "uhash", "x_domain", "timestamp", "batch_nr", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

   # repeated cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 36)
  expect_named(ibmr, c("cp", "classif.ce", "runtime", "uhash", "x_domain", "timestamp", "batch_nr", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

  # different hyperparameters
  at_1 = AutoTuner$new(lrn("classif.rpart", cp = to_tune(0.001, 0.1)), rsmp("holdout"), ms, te, tuner = tuner)
  at_2 = AutoTuner$new(lrn("classif.debug", x = to_tune()), rsmp("holdout"), ms, te, tuner = tuner)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 12)
  expect_named(ibmr, c("cp", "x", "classif.ce", "runtime", "uhash", "x_domain", "timestamp", "batch_nr", "iteration", "experiment"))
  expect_equal(unique(ibmr$experiment), c(1, 2))

  # error handling
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = FALSE)

  expect_error(extract_inner_tuning_archives(rr),
    regex = "Set `store_models = TRUE` in `resample()` or `benchmark()`",
    fixed = TRUE)

  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space,
    store_tuning_instance = FALSE, store_benchmark_result = FALSE)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  expect_error(extract_inner_tuning_archives(rr),
    regex = "Set `store_tuning_instance = TRUE` in <AutoTuner:classif.rpart.tuned>.",
    fixed = TRUE)

  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = FALSE)

  expect_error(extract_inner_tuning_archives(bmr), regex = "store_models")

  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space,
    store_tuning_instance = FALSE, store_benchmark_result = FALSE)
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  expect_error(extract_inner_tuning_archives(bmr), regex = "store_tuning_instance")
})
