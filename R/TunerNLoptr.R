#' @title Hyperparameter Tuning with Non-linear Optimization
#'
#' @name mlr_tuners_nloptr
#' @include Tuner.R
#'
#' @description
#' `TunerNLoptr` class that implements non-linear optimization. Calls
#' [nloptr::nloptr] from package \CRANpkg{nloptr}.
#'
#' @details
#' The termination conditions `stopval`, `maxtime` and `maxeval` of
#' [nloptr::nloptr()] are deactivated and replaced by the [bbotk::Terminator]
#' subclasses. The x and function value tolerance termination conditions
#' (`xtol_rel = 10^-4`, `xtol_abs = rep(0.0, length(x0))`,
#' `ftol_rel = 0.0` and `ftol_abs = 0.0`) are still available and implemented with
#' their package defaults. To deactivate these conditions, set them to `-1`.
#'
#' @templateVar id nloptr
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerNLoptr Parameters
#' @inheritSection bbotk::OptimizerNLoptr Progress Bars
#'
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerNLoptr
#' @template section_optimizer
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @source
#' `r format_bib("johnson_2014")`
#'
#' @export
#' @examples
#' \dontrun{
#' # retrieve task
#' task = tsk("pima")
#'
#' # load learner and set search space
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#'
#' # hyperparameter tuning on the pima indians diabetes data set
#' instance = tune(
#'   method = "nloptr",
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   algorithm = "NLOPT_LN_BOBYQA"
#' )
#'
#' # best performing hyperparameter configuration
#' instance$result
#'
#' # all evaluated hyperparameter configuration
#' as.data.table(instance$archive)
#'
#' # fit final model on complete data set
#' learner$param_set$values = instance$result_learner_param_vals
#' learner$train(task)
#' }
TunerNLoptr = R6Class("TunerNLoptr",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerNLoptr$new(),
        man = "mlr3tuning::mlr_tuners_nloptr"
      )
    }
  )
)

mlr_tuners$add("nloptr", TunerNLoptr)
