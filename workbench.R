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
  list(msr("classif.tpr"), msr("classif.fpr")),
  ps,
  term("evals", 10)
)

tuner = TunerRandomSearch$new()
tuner$tune(inst)

print(inst$archive())

