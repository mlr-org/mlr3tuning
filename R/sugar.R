#' @title Syntactic Sugar for Tuner and Terminator Construction
#'
#' @description
#' This function complements [mlr_tuners] and [mlr_terminators] with functions in the spirit
#' of [mlr3::mlr_sugar].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return [Tuner] for `tnr()` and [Terminator] for `term()`.
#' @export
#' @examples
#' term("evals", n_evals = 10)
#' tnr("random_search")
tnr = function(.key, ...) {
  dictionary_sugar(mlr_tuners, .key, ...)
}

#' @rdname tnr
#' @export
term = function(.key, ...) {
  dictionary_sugar(mlr_terminators, .key, ...)
}

