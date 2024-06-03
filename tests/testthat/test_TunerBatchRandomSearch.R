# test_that("TunerRandomSearch", {
#   test_tuner("random_search")
#   test_tuner_dependencies("random_search")
# })

test_that("internal tuning: no primary search space", {
  ti = tune(
    tuner = tnr("random_search"),
    learner = lrn("classif.debug", iter = to_tune(upper = 1000, internal = TRUE),
      early_stopping = TRUE,
      validate = "test"
      ),
    task = tsk("iris"),
    resampling = rsmp("holdout")
  )

  expect_class(ti, "TuningInstanceSingleCrit")
})
