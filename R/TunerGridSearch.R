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
#' # see ?Tuner
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
