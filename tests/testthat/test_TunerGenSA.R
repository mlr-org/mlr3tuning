context("TunerGenSA")


test_that("TunerGenSA", {
  n_evals = 5
  n_folds = 2

  task = mlr3::mlr_tasks$get("iris")

  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_set$values$minsplit = 3

  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = n_folds)

  measures = mlr3::mlr_measures$mget("classif.ce")
  task$measures = measures

  terminator = TerminatorEvaluations$new(n_evals)
  param_set = paradox::ParamSet$new(params = list(
    paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  ff = FitnessFunction$new(task, learner, resampling, param_set)
  gs = TunerGenSA$new(ff, terminator = terminator, temperature = 200)
  expect_equal(gs$settings$temperature, 200)

  result = gs$tune()$tune_result()
  bmr = gs$ff$bmr
  expect_r6(gs, "TunerGenSA")
  expect_data_table(bmr$data, nrow = n_folds * n_evals)
  expect_equal(bmr$data[, uniqueN(hash)], n_evals)
  result = gs$tune()$tune_result()
  expect_list(result)
  expect_number(result$performance["classif.ce"], lower = measures$classif.ce$range[1], upper = measures$classif.ce$range[2])
  expect_list(result$values, len = 3)
  expect_equal(result$values$minsplit, 3)
})
