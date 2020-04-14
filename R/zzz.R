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

  assign("lg", lgr::get_logger("mlr3/mlr3tuning"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end
