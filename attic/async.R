library(mlr3tuningspaces)
library(mlr3learners)
devtools::load_all("../bbotk")
devtools::load_all(".")
source("attic/TunerRandomSearchAsync.R")

lgr::get_logger("mlr3")$set_threshold("warn")

# retrieve task
task = tsk("pima")

# load learner and set search space
pv = lts("classif.xgboost.default")$values[c("eta", "nrounds")]
learner = lrn("classif.xgboost", eval_metric = "logloss")
learner$param_set$values = insert_named(learner$param_set$values, pv)

# hyperparameter tuning on the pima indians diabetes data set
instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 10)
)

library(future)
plan(multisession)

tuner = TunerRandomSearchAsync$new()
tuner$optimize(instance)
