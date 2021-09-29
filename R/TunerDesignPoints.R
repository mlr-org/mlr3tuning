#' @title Hyperparameter Tuning with via Design Points
#'
#' @name mlr_tuners_design_points
#'
#' @description
#' Subclass for tuning w.r.t. fixed design points.
#'
#' We simply search over a set of points fully specified by the user. The points
#' in the design are evaluated in order as given.
#'
#' @templateVar id design_points
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerDesignPoints Parameters
#' @inheritSection bbotk::OptimizerDesignPoints Progress Bars
#' 
#' @template section_parallelization
#' @template section_logging
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @examples
#' library(data.table)
#' 
#' # retrieve task
#' task = tsk("pima")
#' 
#' # load learner and set search space
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#' 
#' # hyperparameter tuning on the pima indians diabetes data set
#' instance = tune(
#'   method = "design_points",
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   design = data.table(cp = c(log(1e-1), log(1e-2)))
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
TunerDesignPoints = R6Class("TunerDesignPoints",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerDesignPoints$new()
      )
    }
  )
)

mlr_tuners$add("design_points", TunerDesignPoints)
