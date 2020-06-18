context("TuningInstanceMulticrit")

test_that("tuning with multiple objectives", {

  task = tsk("pima")
  resampling = rsmp("holdout")
  learner = lrn("classif.rpart")

  measure_ids = c("classif.fpr", "classif.tpr")
  measures = lapply(measure_ids, mlr3::msr)
  
  tune_ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))

  terminator = term("evals", n_evals = 10)
  tuner = tnr("random_search")

  inst = TuningInstanceMulticrit$new(task, learner, resampling, measures, tune_ps, terminator)

  # This still triggers an error 
  tuner$optimize(inst)

  sp = inst$result_x_seach_space
  obj = inst$result_y

  expect_names(names(sp), identical.to = tune_ps$ids())
  expect_data_table(sp, min.rows = 1, ncols = length(measures))
  expect_names(names(obj), identical.to = measure_ids)
})
