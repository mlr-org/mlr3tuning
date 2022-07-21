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

  x = utils::getFromNamespace("bbotk_reflections", ns = "bbotk")
  x$callback_steps = c(x$callback_steps, "on_eval_after_design", "on_eval_after_benchmark", "on_eval_after_aggregation")

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

leanify_package()
