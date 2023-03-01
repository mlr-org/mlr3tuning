#' @title Hyperparameter Tuning with Design Points
#'
#' @name mlr_tuners_design_points
#'
#' @description
#' Subclass for tuning w.r.t. fixed design points.
#'
#' We simply search over a set of points fully specified by the user.
#' The points in the design are evaluated in order as given.
#'
#' @templateVar id design_points
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerDesignPoints Parameters
#' @inheritSection Tuner Resources
#' @inheritSection bbotk::OptimizerDesignPoints Progress Bars
#' @template section_parallelization
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerDesignPoints
#' @template section_optimizer
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @examples
#' # Hyperparameter Optimization
#'
#' # load learner and set search space
#' learner = lrn("classif.rpart",
#'   cp = to_tune(1e-04, 1e-1),
#'   minsplit = to_tune(2, 128),
#'   minbucket = to_tune(1, 64)
#' )
#'
#' # create design
#' design = mlr3misc::rowwise_table(
#'   ~cp,   ~minsplit,  ~minbucket,
#'   0.1,   2,          64,
#'   0.01,  64,         32,
#'   0.001, 128,        1
#' )
#'
#' # run hyperparameter tuning on the Palmer Penguins data set
#' instance = tune(
#'   tuner = tnr("design_points", design = design),
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
TunerDesignPoints = R6Class("TunerDesignPoints",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        optimizer = OptimizerDesignPoints$new(),
        man = "mlr3tuning::mlr_tuners_design_points"
      )
    }
  )
)

mlr_tuners$add("design_points", TunerDesignPoints)
