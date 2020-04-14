#' @title TunerNloptr
#'
#' @name mlr_tuners_nloptr
#' @include Tuner.R
#'
#' @description
#' Subclass for general nonlinear optimization procedures [nloptr::nloptr()] from package \CRANpkg{nloptr}.
#'
#' @section Parameters:
#' * `algorithm` (`character(1)`).
#' * `xtol_rel` (`numeric(1)`).
#' * `xtol_abs` (`numeric(1)`).
#' * `ftol_rel` (`numeric(1)`).
#' * `ftol_abs` (`numeric(1)`).
#' * `eval_g_eq` (`function).
#' * `eval_g_ineq` (`function`).
#'
#' For the meaning of the control parameters, see [nloptr::nloptr()] and the [nlopt documentation](https://nlopt.readthedocs.io/en/latest/).
#' Note that we have removed all control parameters which refer to the termination of the algorithm and
#' where our terminators allow to obtain the same behavior.
#' Note that no gradient information is passed to [nloptr::nloptr()], and thus in general `algorithm`
#' settings that require gradients do not work.
#'
#' @templateVar id nloptr
#' @template section_dictionary_tuners
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerNloptr = R6Class("TunerNloptr", inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(params = list(
        ParamFct$new("algorithm", default = "NLOPT_LN_COBYLA", tags = "opts",
          levels = strsplit(nloptr::nloptr.get.default.options()[1, "possible_values"], ", ")[[1]]),
        ParamDbl$new("xtol_rel", default = 0, lower = 0, upper = Inf, tags = "opts"),
        ParamDbl$new("xtol_abs", default = 0, lower = 0, upper = Inf, tags = "opts"),
        ParamDbl$new("ftol_rel", default = 0, lower = 0, upper = Inf, tags = "opts"),
        ParamDbl$new("ftol_abs", default = 0, lower = 0, upper = Inf, tags = "opts"),
        ParamUty$new("eval_g_eq", default = function(x) 0, tags = "constraints"),
        ParamUty$new("eval_g_ineq", default = function(x) 0, tags = "constraints")
      ))
      ps$values = list(algorithm = "NLOPT_LN_BOBYQA", xtol_rel = .Machine$double.eps)
      super$initialize(
        param_set = ps,
        param_classes = "ParamDbl",
        properties = character(),
        packages = "nloptr"
      )
    }
  ),

  private = list(
    .tune = function(instance) {
      v = self$param_set$values
      # nloptr requires a starting point x0. 
      # We either start from a randomly sampled config or the current best found.
      x0 = instance$result$tune_x %??% generate_design_random(instance$param_set, 1L)$data
      opt = nloptr::nloptr(
        x0 = unlist(x0),
        eval_f = function(x) {instance$tuner_objective(setNames(x, instance$param_set$ids()))},
        lb = instance$param_set$lower,
        ub = instance$param_set$upper,
        eval_g_ineq = v$eval_g_ineq,
        opts = self$param_set$get_values(tags = "opts"))
      opt$solution
    }
  )
)

mlr_tuners$add("nloptr", TunerNloptr)
