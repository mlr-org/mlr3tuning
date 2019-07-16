#' @title TunerGridSearch
#'
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Tuner child class to conduct a grid search.
#'
#' @section Construction:
#' ```
#' tuner = TunerGridSearch$new(pe, terminator, resolution)
#' ```
#' For arguments, see [Tuner], and additionally:
#'
#' * `resolution` :: `integer(1)`\cr
#'   Resolution of the grid.
#'   If none is specified we will try to calculate the resolution form the Terminator.
#'
#' @section Fields:
#' See [Tuner].
#'
#' @section Methods:
#' See [Tuner].
#'
#' @family Tuner
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' resampling = mlr_resamplings$get("cv")
#' resampling$param_set$values$folds = 2
#' measures = mlr_measures$mget("classif.ce")
#' param_set = ParamSet$new(
#'   params = list(
#'     ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#'   )
#' )
#' pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set)
#'
#' terminator = TerminatorEvaluations$new(10)
#' gs = TunerGridSearch$new(pe, terminator)
#' gs$tune()$tune_result()
TunerGridSearch = R6Class("TunerGridSearch",
  inherit = Tuner,
  public = list(
    resolution = NULL,
    grid = NULL,

    initialize = function(pe, terminator = NULL, resolution = 10L, batchsize = 10L) {
      resolution = assert_int(resolution, lower = 1L, coerce = TRUE)
      grid = generate_design_grid(pe$param_set, resolution = resolution)
      terminator = TerminatorEvaluations$new(nrow(grid$data))
      super$initialize(id = "grid_search", pe = pe, terminator = terminator)
      self$resolution = resolution
      self$grid = grid
      return(self)
    }
  ),

  private = list(
    tune_step = function() {
      private$eval_design_terminator(self$grid)
    }
  )
)
