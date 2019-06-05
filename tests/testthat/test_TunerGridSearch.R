context("TunerGridSearch")


test_that("TunerGridSearch", {
  task = mlr3::mlr_tasks$get("iris")

  learner = mlr3::mlr_learners$get("classif.rpart")
  learner$param_set$values = list(minsplit = 3)

  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_set$values = list(folds = 2)

  measures = mlr3::mlr_measures$mget("classif.ce")
  task$measures = measures

  terminator = TerminatorEvaluations$new(5)
  terminator_false = TerminatorRuntime$new(2, "mins")
  param_set = paradox::ParamSet$new(params = list(
    paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1
  )))

  pe = PerformanceEvaluator$new(task, learner, resampling, param_set)
  expect_error(TunerGridSearch$new(pe, terminator = terminator_false))
  gs = TunerGridSearch$new(pe, terminator = terminator)

  result = gs$tune()$tune_result()
  bmr = gs$pe$bmr
  expect_r6(gs, "TunerGridSearch")
  expect_data_table(bmr$data, nrow = 2 * 5)
  expect_equal(bmr$data[, uniqueN(hash)], 5)
  expect_equal(gs$settings$resolution, 5)
  result = gs$tune()$tune_result()
  expect_list(result)
  expect_number(result$performance["classif.ce"], lower = measures$classif.ce$range[1], upper = measures$classif.ce$range[2])
  expect_list(result$values, len = 2)
  expect_equal(result$values$minsplit, 3)
})

test_that("Design resolution of grid search is correct", {
  pe = PerformanceEvaluator$new(
    task = mlr3::mlr_tasks$get("iris"),
    learner = mlr3::mlr_learners$get("classif.rpart"),
    mlr3::mlr_resamplings$get("holdout"),
    paradox::ParamSet$new(list(
      paradox::ParamDbl$new("cp", 0, 1),
      paradox::ParamInt$new("minsplit", 1, 20),
      paradox::ParamInt$new("maxcompete", 0, 20))))


  expect_output({
    tune = TunerGridSearch$new(pe, TerminatorEvaluations$new(30))$tune()
  })
  r = tune$aggregated(FALSE)
  param_data = unnest(r[, "pars"], "pars")

  design = paradox::generate_design_grid(pe$param_set, resolution = 3)

  expect_equal(remove_named(param_data, "xval"), design$data)
})
