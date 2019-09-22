#' @title Dictionary of Tuners
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3misc::Dictionary].
#'
#' @description
#' A simple [mlr3misc::Dictionary] storing objects of class [Tuner].
#' Each tuner has an associated help page, see `mlr_tuners_[id]`.
#'
#' This dictionary can get populated with additional tuners by add-on packages.
#'
#' For a more convenient way to retrieve and construct tuner, see [tnr()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @family Tuner
#' @seealso
#' Sugar function: [tnr()]
#' @export
#' @examples
#' mlr_tuners$get("grid_search")
#' tnr("random_search")
mlr_tuners = R6Class("DictionaryTuner",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

