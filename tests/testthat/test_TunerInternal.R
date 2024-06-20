test_that("tuner internal works", {

  measure = msr("internal_valid_score", minimize = FALSE, select = "acc")
  instance = ti(
    task = tsk("pima"),
    learner = lrn("classif.debug",
      validate = 0.2,
      early_stopping = TRUE,
      iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99)
    ),
    resampling = rsmp("holdout"),
    measures = measure,
    terminator = trm("evals", n_evals = 10)
  )

  tuner = tnr("internal")

  res = tuner$optimize(instance)
  expect_data_table(res)
  expect_equal(names(res$internal_tuned_values[[1L]]), "iter")
})
