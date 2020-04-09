#' @title TunerCMAES
#'
#' @name mlr_tuners_cmaes
#'
#' @description
#' Subclass for generalized simulated annealing tuning calling [cmaes::cma_es()]
#' from package \CRANpkg{cmaes}.
#'
#' @templateVar id cmaes
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`smooth`}{`logical(1)`}
#' \item{`temperature`}{`numeric(1)`}
#' \item{`acceptance.param`}{`numeric(1)`}
#' \item{`verbose`}{`logical(1)`}
#' \item{`trace.mat`}{`logical(1)`}
#' }
#'
#' For the meaning of the control parameters, see [cmaes::cma_es()]. Note that we
#' have removed all control parameters which refer to the termination of the
#' algorithm and where our terminators allow to obtain the same behavior.
#'
#' @export
#' @template example
TunerCMAES = R6Class("TunerCMAES", inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamUty$new("sigma"), #can be vector of d
        ParamInt$new("mu", lower = 1), #default not documented,
        ParamUty$new("weights"), #type not documented
        ParamUty$new("damps"),
        ParamDbl$new("cs"),
        ParamDbl$new("ccum")
      ))
      super$initialize(
        param_set = ps,
        param_classes = "ParamDbl",
        properties = character(),
        packages = "cmaes"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      v = self$param_set$values
      v$maxit = .Machine$integer.max # make sure cmaes does not stop
      v$vectorized = TRUE
      start = (inst$search_space$lower + inst$search_space$upper / 2)
      cmaes::cma_es(
        par = start,
        fn = objective_wrapper_cmaes,
        lower = inst$search_space$lower,
        upper = inst$search_space$upper,
        control = v,
        inst = inst
      )
    }
  )
)

objective_wrapper_cmaes = function(x, inst) {
  x = as.data.table(t(x))
  res = inst$eval_batch(x)
  y = as.numeric(res[, inst$objective$codomain$ids()[1], with=FALSE][[1]])
  if(inst$objective$codomain$tags[[1]] == "minimize") y else -y
}

mlr_tuners$add("cmaes", TunerCMAES)
