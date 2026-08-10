test_that("TunerBatchCmaes", {
  skip_if_not_installed("libcmaesr")

  expect_tuner(tnr("cmaes"))

  learner = lrn(
    "classif.rpart",
    cp = to_tune(1e-04, 1e-1, logscale = TRUE),
    minsplit = to_tune(p_dbl(2, 128, trafo = as.integer)),
    minbucket = to_tune(p_dbl(1, 64, trafo = as.integer))
  )

  instance = tune(
    tuner = tnr("cmaes", lambda = 5L),
    task = tsk("sonar"),
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    term_evals = 10
  )

  expect_equal(instance$archive$n_evals, 10)
  # one batch per generation
  expect_equal(instance$archive$n_batch, 2L)
  expect_named(instance$result_x_domain, c("cp", "minsplit", "minbucket"), ignore.order = TRUE)
})

test_that("TunerBatchCmaes exceeds the evaluation budget by up to lambda - 1 points", {
  skip_if_not_installed("libcmaesr")

  learner = lrn(
    "classif.rpart",
    cp = to_tune(1e-04, 1e-1, logscale = TRUE),
    minsplit = to_tune(p_dbl(2, 128, trafo = as.integer))
  )

  instance = tune(
    tuner = tnr("cmaes", lambda = 4L),
    task = tsk("sonar"),
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    term_evals = 6
  )

  expect_equal(instance$archive$n_evals, 8)
})
