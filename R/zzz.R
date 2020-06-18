#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import bbotk
#' @importFrom R6 R6Class
"_PACKAGE"


register_bbotk = function() {
  x = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")

  x$add("gensa", OptimizerGenSA)
  x$add("design_points", OptimizerDesignPoints)
  x$add("grid_search", OptimizerGridSearch)
}

.onLoad = function(libname, pkgname) {
  # nocov start
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$tuner_properties = "dependencies"

  register_bbotk()
  setHook(packageEvent("bbotk", "onLoad"), function(...) register_bbotk(), action = "append")

  assign("lg", lgr::get_logger("mlr3/mlr3tuning"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

.onUnload = function(libpath) { # nolint
  event = packageEvent("bbotk", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3tuning"], action = "replace")
} # nocov end
