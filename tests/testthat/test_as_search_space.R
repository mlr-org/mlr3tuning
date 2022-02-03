test_that("as_search_space on Learner works", {
  learner = lrn("classif.rpart", cp = to_tune(1e-3, 1e-1))
  search_space = as_search_space(learner)
  expect_r6(search_space, "ParamSet")
  expect_equal(search_space$ids(), "cp")
})

test_that("as_search_space on ParamSet works", {
  param_set = ps(cp = p_dbl(lower = 1e-4, upper = 1), minsplit = p_int(1, 20))
  search_space = as_search_space(param_set)
  expect_r6(search_space, "ParamSet")
  expect_equal(search_space$ids(), c("cp", "minsplit"))

  param_set$values$cp = to_tune(1e-3, 1e-1)
  search_space = as_search_space(param_set)
  expect_r6(search_space, "ParamSet")
  expect_equal(search_space$ids(), "cp")
})
