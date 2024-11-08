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
  expect_named(irr, c("iteration", "cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "errors", "warnings" ), ignore.order = TRUE)

  # repeated cv
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  irr = extract_inner_tuning_archives(rr)
  expect_data_table(irr, nrows = 18)
  expect_named(irr, c("iteration", "cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "errors", "warnings"), ignore.order = TRUE)

  # cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space, id = "at_1")
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space, id = "at_2")
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 12)
  expect_named(ibmr, c("experiment", "iteration", "cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "errors", "warnings"), ignore.order = TRUE)
  expect_equal(unique(ibmr$experiment), c(1, 2))

   # repeated cv
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space, id = "at_1")
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space, id = "at_2")
  resampling_outer = rsmp("repeated_cv", folds = 2, repeats = 3)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 36)
  expect_named(ibmr, c("experiment", "iteration", "cp", "classif.ce", "x_domain_cp", "runtime_learners", "timestamp", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "errors", "warnings"), ignore.order = TRUE)
  expect_equal(unique(ibmr$experiment), c(1, 2))

  # different hyperparameters
  at_1 = AutoTuner$new(lrn("classif.rpart", cp = to_tune(0.001, 0.1)), rsmp("holdout"), ms, te, tuner = tuner)
  at_2 = AutoTuner$new(lrn("classif.debug", x = to_tune()), rsmp("holdout"), ms, te, tuner = tuner)
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 12)
  expect_named(ibmr, c("experiment", "iteration", "cp", "x", "classif.ce", "x_domain_cp", "x_domain_x", "runtime_learners", "timestamp", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "errors", "warnings"), ignore.order = TRUE)
  expect_equal(unique(ibmr$experiment), c(1, 2))

  # no models
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = FALSE)

  expect_data_table(extract_inner_tuning_archives(rr), nrows = 0, ncols = 0)

  # no instance
  at = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space,
    store_tuning_instance = FALSE, store_benchmark_result = FALSE)
  resampling_outer = rsmp("cv", folds = 2)
  rr = resample(task, at, resampling_outer, store_models = TRUE)

  expect_data_table(extract_inner_tuning_archives(rr), nrows = 0, ncols = 0)

  # no models
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space, id = "at_1")
  at_2 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space, id = "at_2")
  resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = FALSE)

  expect_data_table(extract_inner_tuning_archives(rr), nrows = 0, ncols = 0)

  # mixed store instance
  at_1 = AutoTuner$new(lrn("classif.rpart"), rsmp("holdout"), ms, te, tuner = tuner, search_space,
    store_tuning_instance = FALSE, store_benchmark_result = FALSE)
  at_2 = AutoTuner$new(lrn("classif.debug", x = to_tune()), rsmp("holdout"), ms, te, tuner = tuner)
    resampling_outer = rsmp("cv", folds = 2)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 6)
  expect_named(ibmr, c("experiment", "iteration", "x", "classif.ce", "x_domain_x", "runtime_learners", "timestamp", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "errors", "warnings"), ignore.order = TRUE)
  expect_equal(unique(ibmr$experiment), 2)

  # autotuner and learner
  learner = lrn("classif.rpart")
  at = AutoTuner$new(lrn("classif.debug", x = to_tune()), rsmp("holdout"), ms, te, tuner = tuner)
  grid = benchmark_grid(task, list(at, learner), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 6)
  expect_named(ibmr, c("experiment", "iteration", "x", "classif.ce", "x_domain_x", "runtime_learners", "timestamp", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "errors", "warnings"), ignore.order = TRUE)
  expect_equal(unique(ibmr$experiment), 1)

  # search_space > 1
  at_1 = AutoTuner$new(lrn("classif.rpart", cp = to_tune(0.01, 0.1), minsplit = to_tune(1, 12)), rsmp("holdout"), ms, te, tuner = tuner)
  at_2 = AutoTuner$new(lrn("classif.debug", x = to_tune()), rsmp("holdout"), ms, te, tuner = tuner)
  grid = benchmark_grid(task, list(at_1, at_2), resampling_outer)
  bmr = benchmark(grid, store_models = TRUE)

  ibmr = extract_inner_tuning_archives(bmr)
  expect_data_table(ibmr, nrows = 14)
  expect_named(ibmr, c("experiment", "iteration", "cp", "minsplit", "x", "classif.ce", "x_domain_cp", "x_domain_minsplit", "x_domain_x", "runtime_learners", "timestamp", "batch_nr", "resample_result", "task_id", "learner_id", "resampling_id", "errors", "warnings"), ignore.order = TRUE)
  expect_equal(unique(ibmr$experiment), c(1, 2))
})

test_that("works with internal tuning", {
  at = auto_tuner(
    tuner = tnr("random_search", batch_size = 2),
    learner = lrn("classif.debug", iter = to_tune(upper = 1000L, internal = TRUE), x = to_tune(0.2, 0.3), early_stopping = TRUE, validate = "test"),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    term_evals = 4
  )

  bmr = benchmark(benchmark_grid(tsk("iris"), at, rsmp("cv", folds = 2L)), store_models = TRUE)

  ita = extract_inner_tuning_archives(bmr)
  expect_list(ita$internal_tuned_values)
  itr = extract_inner_tuning_results(bmr)
  expect_list(itr$internal_tuned_values)
})
