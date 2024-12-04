#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import bbotk
#' @importFrom R6 R6Class
#' @importFrom utils tail
#' @importFrom stats sd
"_PACKAGE"

.onLoad = function(libname, pkgname) {
  # nocov start
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$tuner_properties = "dependencies"

  # callbacks
  x = utils::getFromNamespace("mlr_callbacks", ns = "mlr3misc")
  x$add("mlr3tuning.async_default_configuration", load_callback_async_default_configuration)
  x$add("mlr3tuning.async_measures", load_callback_async_measures)
  x$add("mlr3tuning.async_mlflow", load_callback_async_mlflow)
  x$add("mlr3tuning.async_save_logs", load_callback_async_save_logs)
  x$add("mlr3tuning.async_one_se_rule", load_callback_async_one_se_rule)
  x$add("mlr3tuning.async_freeze_archive", load_callback_freeze_archive)
  x$add("mlr3tuning.backup", load_callback_backup)
  x$add("mlr3tuning.default_configuration", load_callback_default_configuration)
  x$add("mlr3tuning.measures", load_callback_measures)
  x$add("mlr3tuning.one_se_rule", load_callback_one_se_rule)

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end

leanify_package()
