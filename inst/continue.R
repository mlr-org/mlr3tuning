devtools::load_all("/home/marc/Repository/mlr3")
devtools::load_all("/home/marc/Repository/mlr3learners")
devtools::load_all("/home/marc/Repository/bbotk")
devtools::load_all(".")
devtools::load_all("/home/marc/Repository/mlr3hyperband")

library(data.table)
library(paradox)
library(R6)
library(tictoc)



task = tsk("spam")
learner = lrn("classif.xgboost")
measure = msr("classif.ce")
resampling = rsmp("cv", folds = 3)
terminator = trm("evals", n_evals = 100)

search_space = ParamSet$new(list(
  ParamDbl$new("nrounds", lower = 200, upper = 1000, tag = "budget"),
  ParamDbl$new("eta", lower = 0, upper = 1)
))

inst = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = resampling,
  measure = measure,
  search_space = search_space,
  terminator = terminator,
  store_benchmark_result = TRUE,
  store_models = TRUE,
  check_values = TRUE)

tuner = tnr("hyperband")

tic()
tuner$optimize(inst)
toc()




