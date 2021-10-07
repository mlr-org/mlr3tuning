test_that("hotstart works", {

  devtools::load_all("../bbotk")
  devtools::load_all(".")
  devtools::load_all("../mlr3tuningspaces")

  task = tsk("pima")
  learner = lrn("classif.xgboost",
    eta = to_tune(1e-4, 1, logscale = TRUE),
    max_depth = to_tune(1, 20),
    nrounds = to_tune(1, 16),
    eval_metric = "logloss"
    )

  instance = tune(
    method = "grid_search",
    task = task,
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    batch_size = 5,
    resolution = 5,
    allow_hotstart = TRUE

  )


})
