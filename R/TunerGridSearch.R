#' @title TunerGridSearch
#'
#' @description
#' TunerGridSearch
#'
#' @section Usage:
#' ```
#' tuner = TunerGridSearch$new(ff, resolution)
#' ```
#' See [Tuner] for a description of the interface.
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the Tuner.
#' * `resolution` (`integer(1)`):
#'   Resolution of the grid.
#'   If none is specified we will try to calculate the resolution form the Terminator.

#' @section Details:
#' `$new()` creates a new object of class [TunerGridSearch].
#' The interface is described in [Tuner].
#'
#' @name TunerGridSearch
#' @keywords internal
#' @family Tuner
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv")
#' measures = mlr3::mlr_measures$mget("mmce")
#' terminator = TerminatorEvaluations$new(10)
#' param_set = paradox::ParamSet$new(params = list(paradox::ParamReal$new("cp", lower = 0.001, upper = 0.1)))
#'
#' ff = FitnessFunction$new(task, learner, resampling, measures, param_set, terminator)
#'
#' rs = TunerGridSearch$new(ff)
#' rs$tune()$tune_result()
NULL

#' @export
#' @include Tuner.R
TunerGridSearch = R6Class("TunerGridSearch",
  inherit = Tuner,
  public = list(
    initialize = function(ff, resolution = NULL) {
      if (is.null(resolution)) {
        remaining = ff$terminator$remaining
        assertInt(remaining, lower = 1L)
        resolution = floor(remaining ^ 1 / ff$param_set$length)
      }
      assertInt(resolution, lower = 1L)
      super$initialize(id = "grid_search", ff = ff, settings = list(resolution = resolution))
    }
  ),

  private = list(
    tune_step = function() {
      # note: generate_grid_design offers param_resolutions, so theoretically we could allow different resolutions per parameter
      xs = self$ff$param_set$generate_grid_design(resolution = self$settings$resolution)
      xs = self$ff$param_set$transform(xs)
      self$ff$eval_vectorized(.mapply(list, xs, list()))
    }
  )
)

