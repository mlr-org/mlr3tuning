#' @title Hyperparameter Tuning with Non-linear Optimization
#'
#' @name mlr_tuners_nloptr
#' @include Tuner.R
#'
#' @description
#' Subclass for non-linear optimization (NLopt).
#' Calls [nloptr::nloptr] from package \CRANpkg{nloptr}.
#'
#' @details
#' The termination conditions `stopval`, `maxtime` and `maxeval` of [nloptr::nloptr()] are deactivated and replaced by the [bbotk::Terminator] subclasses.
#' The x and function value tolerance termination conditions (`xtol_rel = 10^-4`, `xtol_abs = rep(0.0, length(x0))`, `ftol_rel = 0.0` and `ftol_abs = 0.0`) are still available and implemented with their package defaults.
#' To deactivate these conditions, set them to `-1`.
#'
#' @templateVar id nloptr
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerNLoptr Parameters
#' @inheritSection Tuner Resources
#' @inheritSection bbotk::OptimizerNLoptr Progress Bars
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerNLoptr
#' @template section_optimizer
#'
#' @family Tuner
#' @source
#' `r format_bib("johnson_2014")`
#'
#' @export
#' @examples
#' # Hyperparameter Optimization
#' \donttest{
#'
#' # load learner and set search space
#' learner = lrn("classif.rpart",
#'   cp = to_tune(1e-04, 1e-1, logscale = TRUE)
#' )
#'
#' # run hyperparameter tuning on the Palmer Penguins data set
#' instance = tune(
#'   method = tnr("nloptr", algorithm = "NLOPT_LN_BOBYQA"),
#'   task = tsk("penguins"),
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce")
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
#' learner$train(tsk("penguins"))
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
