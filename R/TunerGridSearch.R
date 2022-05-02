#' @title Hyperparameter Tuning with Grid Search
#'
#' @name mlr_tuners_grid_search
#'
#' @description
#' Subclass for grid search tuning.
#'
#' The grid is constructed as a Cartesian product over discretized values per
#' parameter, see [paradox::generate_design_grid()]. If the learner supports
#' hotstarting, the grid is sorted by the hotstart parameter (see also
#' [mlr3::HotstartStack]). If not, the points of the grid are evaluated in a
#' random order.
#'
#' @templateVar id grid_search
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`resolution`}{`integer(1)`\cr
#' Resolution of the grid, see [paradox::generate_design_grid()].}
#' \item{`param_resolutions`}{named `integer()`\cr
#' Resolution per parameter, named by parameter ID, see
#' [paradox::generate_design_grid()].}
#' \item{`batch_size`}{`integer(1)`\cr
#' Maximum number of points to try in a batch.}
#' }
#'
#' @template section_progress_bars
#' @template section_parallelization
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerGridSearch
#' @template section_optimizer
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @template example
TunerGridSearch = R6Class("TunerGridSearch",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        batch_size = p_int(lower = 1L, tags = "required"),
        resolution = p_int(lower = 1L),
        param_resolutions = p_uty()
      )
      param_set$values = list(resolution = 10L, batch_size = 1L)
      super$initialize(
        id = "grid_search",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit"),
        label = "Grid Search",
        man = "mlr3tuning::mlr_tuners_grid_search"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pv = self$param_set$values
      allow_hotstart = inst$objective$allow_hotstart
      data = generate_design_grid(inst$search_space, resolution = pv$resolution,
        param_resolutions = pv$param_resolutions)$data

      if (allow_hotstart) {
        hotstart_id = inst$objective$learner$param_set$ids(tags = "hotstart")
        order =  if ("hotstart_backward" %in% inst$objective$learner$properties) -1L else 1L
        setorderv(data, hotstart_id, order = order)
      }

      ch = chunk_vector(seq_row(data), chunk_size = pv$batch_size, shuffle = !allow_hotstart)
      for (inds in ch) {
        inst$eval_batch(data[inds])
      }
    }
  )
)

mlr_tuners$add("grid_search", TunerGridSearch)
