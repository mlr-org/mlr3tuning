#' @title Hyperparameter Tuning with Covariance Matrix Adaptation Evolution Strategy
#'
#' @name mlr_tuners_cmaes
#'
#' @description
#' Subclass for Covariance Matrix Adaptation Evolution Strategy (CMA-ES).
#' Calls [libcmaesr::cmaes()] from package \CRANpkg{libcmaesr}.
#'
#' @templateVar id cmaes
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerBatchCmaes Parameters
#' @inheritSection bbotk::OptimizerBatchCmaes Batch evaluation
#' @inheritSection Tuner Resources
#' @template section_progress_bars
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerBatchCmaes
#' @template section_optimizer
#'
#' @source
#' `r format_bib("hansen_2016")`
#'
#' @family Tuner
#' @export
#' @examples
#' # example only runs if libcmaesr is available
#' if (mlr3misc::require_namespaces("libcmaesr", quietly = TRUE)) {
#' # Hyperparameter Optimization
#'
#' # load learner and set search space
#' learner = lrn("classif.rpart",
#'   cp = to_tune(1e-04, 1e-1, logscale = TRUE),
#'   minsplit = to_tune(p_dbl(2, 128, trafo = as.integer)),
#'   minbucket = to_tune(p_dbl(1, 64, trafo = as.integer))
#' )
#'
#' # run hyperparameter tuning on the Palmer Penguins data set
#' instance = tune(
#'   tuner = tnr("cmaes"),
#'   task = tsk("penguins"),
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 10)
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
TunerBatchCmaes = R6Class(
  "TunerBatchCmaes",
  inherit = TunerBatchFromOptimizerBatch,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerBatchCmaes$new(),
        man = "mlr3tuning::mlr_tuners_cmaes"
      )
    }
  )
)

mlr_tuners$add("cmaes", TunerBatchCmaes)
