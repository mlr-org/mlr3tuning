assert_terminator = function(terminator) {
  assert_r6(terminator, "Terminator")
}

assert_tuner = function(tuner) {
  assert_r6(tuner, "Tuner")
}

# TODO: Move this to checkmate / mlr3misc?
assert_set = function(x, empty = TRUE, .var.name = vname(x)) {
  assert_character(x, min.len = as.integer(!empty), any.missing = FALSE, min.chars = 1L, unique = TRUE, .var.name = .var.name)
}
