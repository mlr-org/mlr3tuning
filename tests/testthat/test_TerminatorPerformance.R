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
    terminator = TerminatorPerformance$new(c(classif.ce = 0.1, false.measure = 0.9), pe)
  })
  expect_error({
    terminator = TerminatorPerformance$new(c(classif.ce = 0.1, classif.acc = 2), pe)
  }, "<=")
  expect_error({
    terminator = TerminatorPerformance$new(c(0.1, 0.9), pe)
  }, "named")
  expect_silent({
    terminator = TerminatorPerformance$new(c(classif.ce = 0.1, classif.acc = 0.9), pe)
  })

  gs = TunerGenSA$new(pe, terminator)
  gs$tune()

  aggr = pe$bmr$aggregate(measures)
  thresh = terminator$settings$thresh
  expect_true(any(aggr[["classif.ce"]] <= thresh["classif.ce"] & aggr[["classif.acc"]] >= thresh["classif.acc"]))
})
