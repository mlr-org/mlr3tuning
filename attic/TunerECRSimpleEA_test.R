setwd("~/cos/mlr3tuning")
library(mlr3)
library(paradox)
library(mlr3learners)

learner = lrn("classif.svm")

ps = ParamSet$new(params = list(
  ParamDbl$new("cost", lower = -10, upper = 10),
  ParamDbl$new("gamma", lower = -10, upper = 10)
))

ps$trafo = function(x, ps) {
    x$cost= 2^x$cost
    x$gamma = 2^x$gamma
    return(x)
}

te = term("evals", n_evals = 10)

inst = TuningInstance$new(tsk("iris"), learner, rsmp("holdout"), msr("classif.ce"), ps, te)
tuner = tnr("ecr", mu = 1, lambda = 1, sdev = 1)
tuner$optimize(inst)
