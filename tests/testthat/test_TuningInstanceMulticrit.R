context("TuningInstanceMulticrit")

test_that("tuning with multiple objectives", {

  task = tsk("pima")
  resampling = rsmp("holdout")
  learner = lrn("classif.rpart")

  measures = list(mlr3::msr("classif.fpr"), mlr3::msr("classif.tpr"))
  
  tune_ps = ParamSet$new(list(
    ParamDbl$new("cp", lower = 0.001, upper = 0.1),
    ParamInt$new("minsplit", lower = 1, upper = 10)
  ))

  terminator = term("evals", n_evals = 10)
  tuner = tnr("random_search")

  inst = TuningInstanceMulticrit$new(task, learner, resampling, measures, tune_ps, terminator)

  # This still triggers an error 
  tuner$optimize(inst)

})
