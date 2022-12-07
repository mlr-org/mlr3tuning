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

test_that("ti interface is equal to TuningInstanceSingleCrit", {
  ti_args = formalArgs(ti)
  ti_args[ti_args == "measures"] = "measure"
  instance_args = formalArgs(TuningInstanceSingleCrit$public_methods$initialize)

  expect_equal(ti_args, instance_args)
})

test_that("ti interface is equal to TuningInstanceMultiCrit", {
  ti_args = formalArgs(ti)
  instance_args = formalArgs(TuningInstanceMultiCrit$public_methods$initialize)

  expect_equal(ti_args, instance_args)
})
