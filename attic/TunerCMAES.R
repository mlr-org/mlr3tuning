#FIXME: ist beschissen dass das alles nicht mu-parallele auswertungen der population erlaubt

TunerCMAES = R6Class("TunerCMAES", inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamDbl$new("sigma", lower = 0, default = 0.5)
      ))
      ps$values = list(sigma = 0.5)
      super$initialize(
        param_set = ps,
        param_classes = "ParamDbl",
        properties = character(),
        packages = "adagio"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      ss = inst$search_space
      v = self$param_set$values
      x0 = (ss$upper + ss$lower) / 2 # lets use center point to start optim
      adagio::pureCMAES(par = x0, fun = objective_wrapper_cmaes, inst = inst,
        lower = ss$lower, upper = ss$upper,
        sigma = v$sigma,
        stopeval = .Machine$integer.max # make sure we dont stop
      )
    }
  )
)

#FIXME: ist das nicht in bbotk drin?
objective_wrapper_cmaes = function(x, inst) {
  x = as.data.table(as.list(x))
  colnames(x) = inst$search_space$ids()
  res = inst$eval_batch(x)
  y = as.numeric(res[, inst$objective$codomain$ids()[1], with=FALSE])
  if(inst$objective$codomain$tags[[1]] == "minimize") y else -y
}

mlr_tuners$add("cmaes", TunerCMAES)

