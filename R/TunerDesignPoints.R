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
#' @template section_parallelization
#' @template section_logging
#'
#' @inheritSection bbotk::OptimizerDesignPoints Parameters
#' @inheritSection bbotk::OptimizerDesignPoints Progress Bars
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' library(data.table)
#' search_space = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' terminator = trm("evals", n_evals = 3)
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = terminator
#' )
#' design = data.table(cp = c(0.1, 0.01))
#' tt = tnr("design_points", design = design)
#' # modifies the instance by reference
#' tt$optimize(instance)
#' # returns best configuration and best performance
#' instance$result
#' # allows access of data.table of full path of all evaluations
#' instance$archive
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
