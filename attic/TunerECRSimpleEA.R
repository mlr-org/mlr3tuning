# FIXME: sinnvolles init für sdev. eigemtlich müsste an in der objective alle params auf [0,1] skalieren.
#  weil es auch nur ein sdev gibt

TunerECRSimpleEA = R6Class("TunerECRSimpleEA", inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamFct$new("survival.strategy", levels = c("plus", "comma")),
        ParamInt$new("mu", lower = 1L),
        ParamInt$new("lambda", lower = 1L),
        ParamDbl$new("sdev", lower = 0)
      ))
      ps$values = list(survival.strategy = "plus", mu = 1L, lambda = 1L, sdev = 1)
      super$initialize(
        param_set = ps,
        param_classes = "ParamDbl",
        properties = character(),
        packages = c("ecr", "ParamHelpers")
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      ss = inst$search_space
      v = self$param_set$values
      ffps = ParamHelpers::makeNumericParamSet(len = ss$length,
        lower = ss$lower, upper = ss$upper)
      ff = smoof::makeSingleObjectiveFunction(objective_wrapper_ecr, par.set = ffps,
        name = "smoof_tune_obj", id = "smoof_tune_obj", description = "")
      # we dont stop with ecr, we stop with mlr3tuning terminators
      ecr_term = list(ecr::stopOnIters(.Machine$integer.max))
      res = ecr::ecr(fitness.fun = ff, inst = inst,
        minimize = TRUE, # always minimize; we negate in objective wrapper
        n.dim = ss$length, lower = ss$lower, upper = ss$upper,
        representation = "float", survival.strategy = v$survival.strategy,
        mu = v$mu, lambda = v$lambda,
        mutator = ecr::setup(ecr::mutGauss, sdev = v$sdev, lower = ss$lower, upper = ss$upper),
        terminators = ecr_term
      )
    }
  )
)

objective_wrapper_ecr = function(x, inst) {
  x = as.data.table(as.list(x))
  colnames(x) = inst$search_space$ids()
  res = inst$eval_batch(x)
  y = as.numeric(res[, inst$objective$codomain$ids()[1], with=FALSE])
  if(inst$objective$codomain$tags[[1]] == "minimize") y else -y
}

mlr_tuners$add("ecr", TunerECRSimpleEA)

