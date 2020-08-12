context("TuningInstanceMultiCrit")

test_that("tuning with multiple objectives", {

  task = tsk("pima")
  resampling = rsmp("holdout")
  learner = lrn("classif.rpart")

  measure_ids = c("classif.fpr", "classif.tpr")
  measures = msrs(measure_ids)

  tune_ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))

  terminator = trm("evals", n_evals = 10)
  tuner = tnr("random_search")

  inst = TuningInstanceMultiCrit$new(task, learner, resampling, measures, tune_ps, terminator)

  tuner$optimize(inst)

  sp = inst$result_x_search_space
  obj = inst$result_y

  expect_names(names(sp), identical.to = tune_ps$ids())
  expect_data_table(sp, min.rows = 1, ncols = length(measures))
  expect_names(names(obj), identical.to = measure_ids)
  expect_data_table(inst$archive$data(), nrows = 10L)
  expect_equal(inst$archive$cols_y, measure_ids)
  expect_data_table(inst$archive$best())
  expect_list(inst$result_x_domain)
})

test_that("store_benchmark_result flag works", {
  inst = TEST_MAKE_INST1_2D(store_benchmark_result = FALSE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_true("uhashes" %nin% colnames(inst$archive$data()))

  inst = TEST_MAKE_INST1_2D(store_benchmark_result = TRUE)
  inst$eval_batch(data.table(cp = c(0.3, 0.25), minsplit = c(3, 4)))
  expect_r6(inst$archive$benchmark_result, "BenchmarkResult")
})

test_that("check_values flag with parameter set dependencies", {
  learner = LearnerRegrDepParams$new()
  learner$param_set$values$xx = "a"
  search_space = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.1, upper = 0.3),
    ParamDbl$new("yy", lower = 0.1, upper = 0.3)
  ))
  terminator = trm("evals", n_evals = 20)
  tuner = tnr("random_search")

  inst = TuningInstanceMultiCrit$new(tsk("boston_housing"), learner,
    rsmp("holdout"), msrs(c("regr.mse", "regr.rmse")), search_space, terminator)
  tuner$optimize(inst)
  expect_named(inst$result_learner_param_vals[[1]], c("xx", "cp", "yy"))

  inst = TuningInstanceMultiCrit$new(tsk("boston_housing"), learner,
                                      rsmp("holdout"), msr("regr.mse"), search_space, terminator,
                                      check_values = TRUE)
  expect_error(tuner$optimize(inst),
    fixed = "The parameter 'yy' can only be set")
})
