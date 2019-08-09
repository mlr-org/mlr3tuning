#' @title TunerGridSearch
#'
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Subclass for grid search tuning.
#'
#' The grid is constructed as a Cartesian product over discretized values per parameter,
#' see [paradox::generate_design_grid].
#' The grid is searched in random order.
#'
#' In order to support general termination criteria and parallelization,
#' we evaluate points in a batch-fashion of size `batch_size`.
#' Number of `batch_size` points are evaluated per iteration (potentially in parallel),
#' then the termination criteria are checked.
#'
#' @section Construction:
#' ```
#' tuner = TunerGridSearch$new(pe, terminator, resolution = 10L, batch_size = 1L)
#' ```
#' For arguments, see [Tuner], and additionally:
#'
#' * `resolution` :: `integer(1)`\cr
#'   Resolution of the grid, see [paradox::generate_design_grid].
#'   Stored in `settings`.
#' * `param_resolutions` :: named `integer()` \cr
#'   Resolution per param, named by parameter ID, see [paradox::generate_design_grid].
#' * `batch_size` :: `integer(1)`\cr
#'   Maximum number of configurations to try in a batch.
#'   Stored in `settings`.
#'
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerGridSearch = R6Class("TunerGridSearch",
  inherit = Tuner,
  public = list(
    initialize = function(pe, terminator = NULL, resolution = 10L, param_resolutions = NULL, batch_size = 1L) {

      # argcheck resolution and param_reolution, code is copy-paste from paradox, but better check the validity in constructor
      ids = pe$param_set$ids()
      ids_num = ids[pe$param_set$is_number]
      par_res = integer(0L) # here we construct the resolution for each param
      if (length(ids_num) > 0L) { # if only categ we dont need to check
        if (is.null(resolution) && is.null(param_resolutions)) {
          stop("You must specify 'resolution' or 'param_resolutions'!")
        }
        if (!is.null(resolution)) {
          # create param_resolutions list, constant entry, same length as ids and named with ids
          resolution = assert_count(resolution, positive = TRUE, coerce = TRUE)
          par_res = set_names(rep.int(resolution, pe$param_set$length), ids)
        }
        if (!is.null(param_resolutions)) {
          assert_integerish(param_resolutions, lower = 1L, any.missing = FALSE, coerce = TRUE)
          assert_names(names(param_resolutions), subset.of = ids_num) # user only needs to pass num params (categ resolutions are overwritten anyway)
          par_res = insert_named(par_res, param_resolutions)
        }
        ids_miss = setdiff(ids_num, names(par_res))
        if (length(ids_miss) > 0L) {
          stopf("Resolution settings missing for some numerical params: %s", str_collapse(ids_miss))
        }
      }
      # argcheck end for resolution

      batch_size = assert_int(batch_size, lower = 1L, coerce = TRUE)
      s = list(batch_size = batch_size, resolution = resolution, param_resolutions = param_resolutions)
      super$initialize(pe = pe, terminator = terminator, settings = s)
      return(self)
    }
  ),

  private = list(
    tune_internal = function() {
      g = generate_design_grid(self$pe$param_set, resolution = self$settings$resolution, param_resolutions = self$settings$param_resolutions)
      ch = chunk_vector(1:nrow(g$data), chunk_size = self$settings$batch_size, shuffle = TRUE)
      for (i in 1:length(ch)) {
        self$eval_batch(g$data[ch[[i]],])
      }
    }
  )
)
