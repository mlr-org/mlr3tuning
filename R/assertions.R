#' @export
assert_tuner = function(tuner) {
  assert_r6(tuner, "Tuner")
}

#' @export
assert_tuners = function(tuners) {
  invisible(lapply(tuners, assert_tuner))
}
