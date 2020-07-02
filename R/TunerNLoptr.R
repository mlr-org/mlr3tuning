#' @title TuneNLoptr
#'
#' @name mlr_tuners_nloptr
#' @include Tuner.R
#'
#' @description
#' `TunerNLoptr` class that implements non-linear optimization. Calls
#' [nloptr::nloptr] from package \CRANpkg{nloptr}.
#'
#' @section Parameters:
#' \describe{
#' \item{`algorithm`}{`character(1)`}
#' \item{`x0`}{`numeric()`}
#' \item{`eval_g_ineq`}{`function()`}
#' \item{`xtol_rel`}{`numeric(1)`}
#' \item{`xtol_abs`}{`numeric(1)`}
#' \item{`ftol_rel`}{`numeric(1)`}
#' \item{`ftol_abs`}{`numeric(1)`}
#' }
#'
#' For the meaning of the control parameters, see [nloptr::nloptr()] and
#' [nloptr::nloptr.print.options()].
#'
#' @details
#' The termination conditions `stopval`, `maxtime` and `maxeval` of
#' [nloptr::nloptr()] are deactivated and replaced by the [Terminator]
#' subclasses. The x and function value tolerance termination conditions
#' (`xtol_rel = 10^-4`, `xtol_abs = rep(0.0, length(x0))`,
#' `ftol_rel = 0.0` and `ftol_abs = 0.0`) are still available and implemented with
#' their package defaults. To deactivate these conditions, set them to `-1`.
#'
#' @templateVar id nloptr
#' @template section_dictionary_tuners
#'
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' library(data.table)
#' search_space = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' # We use the internal termination criterion xtol_rel
#' terminator = term("none")
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = terminator
#' )
#' tt = tnr("nloptr", x0 = 0.1, algorithm = "NLOPT_LN_BOBYQA")
#' # modifies the instance by reference
#' tt$optimize(instance)
#' # returns best configuration and best performance
#' instance$result
#' # allows access of data.table of full path of all evaluations
#' instance$archive
TunerNLoptr = R6Class("TunerNLoptr",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerNLoptr$new()
      )
    }
  )
)

mlr_tuners$add("nloptr", TunerNLoptr)
