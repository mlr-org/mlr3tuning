#' @title Execution control object for tuning
#'
#' @description
#' This function is a wrapper around [mlr3::mlr_control()] with defaults better suited for tuning.
#'
#' @param store_model (`logical(1)`):\cr
#'   Store the models of the individual experiments.
#' @param store_prediction (`logical(1)`):\cr
#'   Store the predictions of the individual experiments.
#' @param verbose (`logical(1)`):\cr
#'   Output additional information.
#' @param ...:\cr
#'   Additional arguments passed down to [mlr3::mlr_control()].
#'
#' @return (list()) with options passed down to [mlr3::benchmark()].
#' @export
tune_control = function(store_model = FALSE, store_prediction = FALSE, verbose = FALSE, ...) {
  mlr3::mlr_control(store_model = store_model, store_prediction = store_prediction, verbose = verbose, ...)
}
