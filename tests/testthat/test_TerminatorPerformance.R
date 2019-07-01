context("TerminatorPerformance")


test_that("TerminatorPerformance", {
  task = mlr3::mlr_tasks$get("iris")
  learner = mlr3::mlr_learners$get("classif.rpart")
  resampling = mlr3::mlr_resamplings$get("cv")
  resampling$param_set$values$folds = 2
  measures = mlr3::mlr_measures$mget(c("classif.ce", "classif.acc"))
  param_set = paradox::ParamSet$new(
    params = list(
      paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)
    )
  )

  pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set)
  expect_error({
    terminator = TerminatorPerformance$new(list(classif.ce = 0.1, false.measure = 0.9), pe)
  })
  expect_error({
    terminator = TerminatorPerformance$new(list(classif.ce = 0.1, classif.acc = 2), pe)
  })
  expect_error({
    terminator = TerminatorPerformance$new(list(0.1, 0.9), pe)
  })
  expect_silent({
    terminator = TerminatorPerformance$new(list(classif.ce = 0.1, classif.acc = 0.9), pe)
  })

  gs = TunerGenSA$new(pe, terminator)
  gs$tune()

  agg = pe$bmr$aggregate(measures)
  expect_equal(terminator$state$msrs_best$classif.ce, min(agg$classif.ce))
  expect_equal(terminator$state$msrs_best$classif.acc, max(agg$classif.acc))
})
