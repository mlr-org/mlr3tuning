#FIXME: this is from mlr3 copypaste but should be in checkmate
check_set = function(x, empty = TRUE) {
  check_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE)
}

assert_set = makeAssertionFunction(check_set)
