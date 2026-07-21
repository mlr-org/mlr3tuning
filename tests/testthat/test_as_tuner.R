test_that("as_tuner without clone returns the same object", {
  tuner = tnr("random_search")
  expect_identical(as_tuner(tuner), tuner)
  expect_identical(as_tuner(tuner, clone = FALSE), tuner)
})

test_that("as_tuner with clone returns a deep clone", {
  tuner = tnr("random_search")
  tuner_clone = as_tuner(tuner, clone = TRUE)
  expect_different_address(tuner, tuner_clone)
  expect_different_address(tuner$param_set, tuner_clone$param_set)

  tuner_clone$param_set$values$batch_size = 99L
  expect_equal(tuner$param_set$values$batch_size, 1L)
})

test_that("as_tuner errors on invalid clone argument", {
  expect_error(as_tuner(tnr("random_search"), clone = "yes"), "clone")
})
