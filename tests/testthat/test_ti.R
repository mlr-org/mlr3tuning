test_that("ti function creates a TuningInstanceBatchSingleCrit", {
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune()),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "TuningInstanceBatchSingleCrit")
})

test_that("ti function creates a TuningInstanceBatchMultiCrit", {
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune()),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "TuningInstanceBatchMultiCrit")
})

test_that("ti interface is equal to TuningInstanceBatchSingleCrit", {
  ti_args = formalArgs(ti)
  ti_args[ti_args == "measures"] = "measure"
  ti_args = ti_args[ti_args != "rush"]
  instance_args = formalArgs(TuningInstanceBatchSingleCrit$public_methods$initialize)

  expect_equal(ti_args, instance_args)
})

test_that("ti interface is equal to TuningInstanceBatchMultiCrit", {
  ti_args = formalArgs(ti)
  ti_args = ti_args[ti_args != "rush"]
  instance_args = formalArgs(TuningInstanceBatchMultiCrit$public_methods$initialize)

  expect_equal(ti_args, instance_args)
})
