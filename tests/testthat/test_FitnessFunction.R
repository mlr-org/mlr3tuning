context("FitnessFunction")

test_that("Construction", {
  task = mlr3::mlr_tasks$get("iris")
  learner = mlr3::mlr_learners$get("classif.rpart")
  resampling = mlr3::mlr_resamplings$get("cv")
  measures = mlr3::mlr_measures$mget("mmce")
  terminator = TerminatorIterations$new("t3", 3)

  ff = FitnessFunction$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measures = measures,
    param_set = ParamSet$new(),
    terminator = terminator
  )

})
