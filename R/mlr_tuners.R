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
#' For a more convenient way to retrieve and construct tuner, see [tnr()]/[tnrs()].
#'
#' @section Methods:
#' See [mlr3misc::Dictionary].
#'
#' @section S3 methods:
#' * `as.data.table(dict, ..., objects = FALSE)`\cr
#'   [mlr3misc::Dictionary] -> [data.table::data.table()]\cr
#'   Returns a [data.table::data.table()] with fields "key", "label", "param_classes", "properties" and "packages" as columns.
#'   If `objects` is set to `TRUE`, the constructed objects are returned in the list column named `object`.
#'
#' @family Dictionary
#' @family Tuner
#' @seealso
#' Sugar functions: [tnr()], [tnrs()]
#'
#' @export
#' @examples
#' as.data.table(mlr_tuners)
#' mlr_tuners$get("random_search")
#' tnr("random_search")
mlr_tuners = R6Class("DictionaryTuner",
  inherit = Dictionary,
  cloneable = FALSE
)$new()

#' @export
as.data.table.DictionaryTuner = function(x, ..., objects = FALSE) {
  assert_flag(objects)

  setkeyv(map_dtr(x$keys(), function(key) {
    t = withCallingHandlers(x$get(key),
      packageNotFoundWarning = function(w) invokeRestart("muffleWarning"))
    insert_named(
      list(key = key, label = t$label, param_classes = list(t$param_classes), properties = list(t$properties), packages = list(t$packages)),
      if (objects) list(object = list(t))
    )
  }, .fill = TRUE), "key")[]
}
