#' @import data.table
#' @import checkmate
#' @import paradox
#' @importFrom mlr3 assert_task assert_learner assert_resampling assert_measures mlr_control
#' @import mlr3misc
#' @importFrom R6 R6Class
"_PACKAGE"


.onLoad = function(libname, pkgname) {
  # nocov start
  assign("lg", lgr::get_logger("mlr3"), envir = parent.env(environment()))
  if (Sys.getenv("IN_PKGDOWN") == "true") {
    lg$set_threshold("warn")
  }
} # nocov end
