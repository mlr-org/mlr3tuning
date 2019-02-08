context("aggregated of Tuner")

test_that("API",  {
  n_folds = 4
  n_evals = 5

  task = mlr3::mlr_tasks$get("iris")

  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)

  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = n_folds)

  measures = mlr3::mlr_measures$mget(c("classif.mmce", "time_train", "time_both"))
  task$measures = measures

  param_set = paradox::ParamSet$new(params = list(
      paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  terminator = TerminatorEvaluations$new(n_evals)
  ff = FitnessFunction$new(task, learner, resampling, param_set)
  rs = TunerRandomSearch$new(ff, terminator)

  expect_error(rs$aggregated())
  rs$tune()
  expect_data_table(rs$aggregated(), nrows = n_evals)
  expect_true("cp" %in% names(rs$aggregated()))
  expect_true("pars" %in% names(rs$aggregated(FALSE)))
})
