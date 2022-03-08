test_that("TunerCmaes", {

expect_tuner(tnr("cmaes"))

  learner = lrn("classif.rpart",
    cp = to_tune(1e-04, 1e-1, logscale = TRUE),
    minsplit = to_tune(p_dbl(2, 128, trafo = as.integer)),
    minbucket = to_tune(p_dbl(1, 64, trafo = as.integer))
  )

  instance = tune(
    method = "cmaes",
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("holdout"),
    measures = msr("classif.ce"),
    term_evals = 10
  )

  expect_equal(instance$archive$n_evals, 10)
  expect_named(instance$result_x_domain, c("cp", "minsplit", "minbucket"))
})
