test_that("ti function creates a TuningInstanceSingleCrit", {
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune()),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "TuningInstanceSingleCrit")
})

test_that("ti function creates a TuningInstanceMultiCrit", {
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune()),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "TuningInstanceMultiCrit")
})
