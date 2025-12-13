#' @title Hyperparameter Tuning with Generalized Simulated Annealing
#'
#' @name mlr_tuners_gensa
#'
#' @description
#' Subclass for generalized simulated annealing tuning.
#' Calls [GenSA::GenSA()] from package \CRANpkg{GenSA}.
#'
#' @details
#' In contrast to the [GenSA::GenSA()] defaults, we set `smooth = FALSE` as a default.
#'
#' @templateVar id gensa
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerBatchGenSA Parameters
#' @inheritSection Tuner Resources
#' @inheritSection bbotk::OptimizerBatchGenSA Progress Bars
#' @template section_parallelization
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerBatchGenSA
#' @template section_optimizer
#'
#' @source
#' `r format_bib("tsallis_1996", "xiang_2013")`
#'
#' @family Tuner
#' @export
#' @examples
#' # example only runs if GenSA is available
#' if (mlr3misc::require_namespaces("GenSA", quietly = TRUE)) {
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
#'   tuner = tnr("gensa"),
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
TunerBatchGenSA = R6Class("TunerBatchGenSA",
  inherit = TunerBatchFromOptimizerBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      optimizer = OptimizerBatchGenSA$new()
      optimizer$param_set$values$smooth = FALSE
      super$initialize(
        optimizer = optimizer,
        man = "mlr3tuning::mlr_tuners_gensa"
      )
    }
  )
)

mlr_tuners$add("gensa", TunerBatchGenSA)
