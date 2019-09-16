devtools::load_all()
library(data.table)

set.seed(123)
lg$set_threshold("error")

# define hyperparameter and budget parameter for tuning with hyperband
ps = ParamSet$new(list(
  ParamDbl$new("cp",       lower = 0.001, upper = 0.1),
  ParamInt$new("minsplit", lower = 1,     upper = 10)
))

inst = TuningInstance$new(
  tsk("pima"),
  lrn("classif.rpart"),
  rsmp("holdout"),
  list(msr("classif.tpr"), msr("classif.fpr"), msr("classif.ce")),
  ps,
  term("evals", 10)
)

tuner = TunerRandomSearch$new()
tuner$tune(inst)

print(inst$archive())
best_result = inst$best()

# all measures of TuningInstance
pareto_result = inst$pareto_front()
# only subset of measures
pareto_result = inst$pareto_front(c("classif.tpr", "classif.fpr"))


