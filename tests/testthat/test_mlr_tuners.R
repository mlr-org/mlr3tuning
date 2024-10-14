test_that("mlr_tuners", {
  skip_if_not_installed(c("rush", "adagio", "GenSA", "irace", "nloptr"))

  expect_dictionary(mlr_tuners, min_items = 1L)
  keys = mlr_tuners$keys()

  for (key in keys) {
    tuner = tnr(key)
    expect_r6(tuner, "Tuner")
  }
})

test_that("mlr_tuners sugar", {
  expect_class(tnr("random_search"), "Tuner")
  expect_class(tnrs(c("random_search", "random_search")), "list")
})

test_that("as.data.table objects parameter", {
  skip_if_not_installed(c("rush", "adagio", "GenSA", "irace", "nloptr"))

  tab = as.data.table(mlr_tuners, objects = TRUE)
  expect_data_table(tab)
  expect_list(tab$object, "Tuner", any.missing = FALSE)
})
