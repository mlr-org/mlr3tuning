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
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv")
#' resampling$param_set$values$folds = 2
#' measures = mlr3::mlr_measures$mget("classif.ce")
#' param_set = paradox::ParamSet$new(
#'   params = list(
#'     paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)
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
    initialize = function(pe, terminator, resolution = NULL) {
      if (is.null(resolution)) {
        remaining = terminator$settings$max_evaluations
        if (is.null(remaining)) {
          stop("Specify resolution or use a terminator that defines maximal number of evaluations (e.g. TerminatorEvaluations).")
        }
        assert_count(remaining, positive = TRUE, coerce = TRUE)
        resolution = floor(remaining^(1 / pe$param_set$length))
      }
      resolution = assert_int(resolution, lower = 1L, coerce = TRUE)
      super$initialize(id = "grid_search", pe = pe, terminator = terminator, settings = list(resolution = resolution))
    }
  ),

  private = list(
    tune_step = function() {
      # note: generate_grid_design offers param_resolutions, so theoretically we could allow different resolutions per parameter
      design = paradox::generate_design_grid(self$pe$param_set, resolution = self$settings$resolution)

      if (inherits(self$terminator, "TerminatorEvaluations")) {
        if (nrow(design$data) < self$terminator$settings$max_evaluations) {
          self$terminator$settings$max_evaluations = nrow(design$data)
          msg_warn = paste0("Set number of maximal evaluations to ", nrow(design$data),
            " to avoid multiple computation of the same grid.")
          lg$warn(msg_warn)
        }
      }
      private$eval_design_terminator(design)
    }
  )
)
