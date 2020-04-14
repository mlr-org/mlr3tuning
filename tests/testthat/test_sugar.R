context("Sugar tuner")

test_that("Sugar tuner", {
  keys = c("design_points",  "gensa", "grid_search", "random_search")
  map(keys, function(key) {
    expect_r6(tnr(key), "Tuner")
  })
})

test_that("Sugar tuner", {
  keys = c("design_points",  "gensa", "grid_search", "random_search")
  map(keys, function(key) {
    expect_r6(tnrs(key)[[1]], "Tuner")
  })
})
