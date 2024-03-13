test_that("AutoTunerRush works", {
  rush_plan(n_workers = 2)

  at = AutoTunerRush$new(
    tuner = tnr("random_search_v2"),
    learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10)
  )

  at$train(tsk("pima"))
})
