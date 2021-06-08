#' @title Syntactic Sugar for Tuner Construction
#'
#' @description
#' This function complements [mlr_tuners] with functions in the spirit
#' of [mlr3::mlr_sugar].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return
#' * [Tuner] for `tnr()`
#' * list of [Tuner] for `tnrs()`
#' @export
#' @examples
#' tnr("random_search")
tnr = function(.key, ...) {
  dictionary_sugar(mlr_tuners, .key, ...)
}

#' @rdname tnr
#' @export
tnrs = function(.keys, ...) {
  dictionary_sugar_mget(mlr_tuners, .keys, ...)
}
