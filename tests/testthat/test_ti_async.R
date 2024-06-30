test_that("ti_async function creates a TuningInstanceAsyncSingleCrit", {
  skip_on_cran()

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune()),
    resampling = rsmp ("holdout"),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "TuningInstanceAsyncSingleCrit")
})

test_that("ti_async function creates a TuningInstanceAsyncMultiCrit", {
  skip_on_cran()

  instance = ti_async(
    task = tsk("pima"),
    learner = lrn("classif.rpart", cp = to_tune()),
    resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")),
    terminator = trm("evals", n_evals = 2))
  expect_class(instance, "TuningInstanceAsyncMultiCrit")
})

test_that("ti_async interface is equal to TuningInstanceAsyncSingleCrit", {
  skip_on_cran()

  ti_args = formalArgs(ti_async)
  ti_args[ti_args == "measures"] = "measure"
  instance_args = formalArgs(TuningInstanceAsyncSingleCrit$public_methods$initialize)

  expect_equal(ti_args, instance_args)
})

test_that("ti_async interface is equal to TuningInstanceAsyncMultiCrit", {
  skip_on_cran()

  ti_args = formalArgs(ti_async)
  instance_args = formalArgs(TuningInstanceAsyncMultiCrit$public_methods$initialize)

  expect_equal(ti_args, instance_args)
})
