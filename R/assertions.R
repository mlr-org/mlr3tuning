assert_tuner = function(tuner) {
  assert_r6(tuner, "Tuner")
}

assert_ro_binding = function(rhs) {
  if (!missing(rhs)) {
    stopf("Field/Binding is read-only")
  }
}
