#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3misc
#' @importFrom R6 R6Class
"_PACKAGE"

register_mlr3 = function() {
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$mlr_control_defaults$store_tuning = FALSE
}

unregister_mlr3 = function() {
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$mlr_control_defaults$store_tuning = NULL
}

.onLoad = function(libname, pkgname) {
  # nocov start
  assign("lg", lgr::get_logger("mlr3"), envir = parent.env(environment()))
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
} # nocov end

.onUnload = function(libpath) {
  # nocov start
  unregister_mlr3()

  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3tuning"], action = "replace")
} # nocov end
