#' @title TunerGridSearchContinue
#'
#' @name mlr_tuners_grid_search_continue
#'
#' @description
#' Subclass for grid search tuning.
#'
#' The grid is constructed as a Cartesian product over discretized values per
#' parameter, see [paradox::generate_design_grid()]. The points of the grid are
#' evaluated in a random order.
#'
#'
#' @templateVar id grid_search_continue
#' @template section_dictionary_tuners
#' @template section_parallelization
#' @template section_logging
#'
#'
#' @section Parameters:
#' \describe{
#' \item{`resolution`}{`integer(1)`\cr
#' Resolution of the grid, see [paradox::generate_design_grid()].}
#' \item{`param_resolutions`}{named `integer()`\cr
#' Resolution per parameter, named by parameter ID, see
#' [paradox::generate_design_grid()].}
#' }
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @examples
#' library(mlr3)
#' library(ml3learners)
#' library(paradox)
#' search_space = ParamSet$new(list(
#'   ParamInt$new("nrounds", lower = 1, upper = 20, tag = "budget"),
#'   ParamDbl$new("colsample_bytree", lower = 0.3, upper = 1)
#' ))
#' terminator = trm("none")
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.xgboost"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = terminator
#' )
#' tt = tnr("grid_search_continue", resolution = 5)
#'
#' # modifies the instance by reference
#' tt$optimize(instance)
#'
#' # returns best configuration and best performance
#' instance$result
#'
#' # allows access of data.table of full path of all evaluations
#' instance$archive
TunerGridSearchContinue = R6Class("TunerGridSearchContinue",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("resolution", lower = 1L),
        ParamUty$new("param_resolutions")
      ))
      ps$values = list(resolution = 10L)
      super$initialize(
        param_set = ps,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      if ("budget" %nin% inst$search_space$tags) {
        stop("No budget parameter")
      }

      pv = self$param_set$values
      data = generate_design_grid(inst$search_space, resolution = pv$resolution,
        param_resolutions = pv$param_resolutions)$data

      budget_id = inst$search_space$ids(tags = "budget")
      ids = inst$search_space$ids()[inst$search_space$ids() %nin% budget_id]

      data[, continue_hash := .GRP, by = ids]
      setorderv(data, cols = c(budget_id, "continue_hash"))
      batch_size = max(data$continue_hash)

      ch = chunk_vector(seq_row(data), chunk_size = batch_size, shuffle = FALSE)
      for (inds in ch) {
        inst$eval_batch(data[inds])
      }
    }
  )
)

mlr_tuners$add("grid_search_continue", TunerGridSearchContinue)
