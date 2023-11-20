#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import bbotk
#' @importFrom R6 R6Class
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$tuner_properties = "dependencies"

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3tuning.early_stopping", load_callback_early_stopping)
  x$add("mlr3tuning.backup", load_callback_backup)
  x$add("mlr3tuning.measures", load_callback_measures)

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

leanify_package()
