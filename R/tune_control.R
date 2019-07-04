#' @title Execution Control Object for Tuning
#'
#' @description
#' This function is an extension of [mlr3::mlr_control()].
#'
#' @param store_tuning (`logical(1)`):\cr
#'   Store internal tuning steps, e.g. the optimization path of the tuner.
#' @param ... :\cr
#'   Additional arguments passed down to [mlr3::mlr_control()].
#'
#' @return (`list()`) with options, passed down to [mlr3::benchmark()].
#' @export
tune_control = function(..., store_tuning = FALSE) {
  insert_named(
    mlr3::mlr_control(...),
    list(store_tuning = assert_flag(store_tuning))
  )
}
