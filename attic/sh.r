devtools::load_all("../mlr3")
devtools::load_all(".")
devtools::load_all("../mlr3hyperband")

task = tsk("pima")

learner = lrn("classif.debug",
  x = to_tune(p_dbl(0.01, 1)),
  iter = to_tune(p_int(1, 4, tags = "budget")))

instance = tune(
  method = "successive_halving",
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  allow_hotstart = TRUE,
  eta = 2,
  n = 4
)

##### xgboost

devtools::load_all("../mlr3")
devtools::load_all(".")
devtools::load_all("../mlr3hyperband")
devtools::load_all("../mlr3learners")
library(mlr3tuningspaces)
library(tictoc)
library(profvis)


task = tsk("pima")

learner = lts(lrn("classif.xgboost"))
learner$param_set$values$nrounds = to_tune(p_int(16, 256, tags = "budget"))
learner$param_set$values$eval_metric = "logloss"

instance = tune(
  method = "successive_halving",
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  allow_hotstart = FALSE,
  eta = 2,
  n = 256
)

###################################




instance = tune(
  method = "successive_halving",
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  allow_hotstart = TRUE,
  eta = 2,
  n = 256
)


profvis({
  instance = tune(
    method = "successive_halving",
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    allow_hotstart = TRUE,
    eta = 2,
    n = 256
  )
})


profvis({
  instance = tune(
    method = "successive_halving",
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    allow_hotstart = FALSE,
    eta = 2,
    n = 256
  )
})
