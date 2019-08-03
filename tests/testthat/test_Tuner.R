context("Tuner")

n_folds = 4
task = mlr_tasks$get("iris")

learner = mlr_learners$get("classif.rpart")
learner$param_set$values = list(minsplit = 3)

resampling = mlr_resamplings$get("cv")
resampling$param_set$values = list(folds = n_folds)

measures = mlr_measures$mget(c("classif.ce", "time_train", "time_both"))

param_set = ParamSet$new(params = list(
  ParamDbl$new("cp", lower = 0.001, upper = 0.1
)))

pe = PerfEval$new(task, learner, resampling, measures, param_set)

test_that("API", {
  for (n_evals in c(1,5)) {
    terminator = TerminatorEvaluations$new(n_evals)
    rs = TunerRandomSearch$new(pe$clone(), terminator)
    expect_error(rs$aggregate())
    rs$tune()
    expect_data_table(rs$aggregate(), nrows = n_evals)
    expect_true("cp" %in% names(rs$aggregate()))
    expect_true("params" %in% names(rs$aggregate(FALSE)))
  }
})
