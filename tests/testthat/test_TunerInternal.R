test_that("tuner internal works", {
  measure = msr("internal_valid_score", minimize = FALSE, select = "acc")
  instance = ti(
    task = tsk("sonar"),
    learner = lrn(
      "classif.debug",
      validate = 0.2,
      early_stopping = TRUE,
      iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99)
    ),
    resampling = rsmp("holdout"),
    measures = measure,
    terminator = trm("evals", n_evals = 10)
  )

  tuner = tnr("internal")

  res = tuner$optimize(instance)
  expect_data_table(res)
  expect_equal(names(res$internal_tuned_values[[1L]]), "iter")
})

test_that("tuning on the best valid score works", {
  measure = msr("best_valid_score", minimize = FALSE, select = "acc")
  instance = ti(
    task = tsk("sonar"),
    learner = lrn(
      "classif.debug",
      validate = 0.2,
      early_stopping = TRUE,
      iter = to_tune(upper = 1000, internal = TRUE, aggr = function(x) 99)
    ),
    resampling = rsmp("holdout"),
    measures = measure,
    terminator = trm("evals", n_evals = 10)
  )

  res = tnr("internal")$optimize(instance)
  expect_data_table(res)
  expect_number(res$acc)
  # the archive is scored with the best, not the final validation score
  expect_true(all(!is.na(instance$archive$data$acc)))
})

test_that("AutoTuner's valid score extractors forward to the tuned learner", {
  # The AutoTuner disables validation for the final model fit, so in practice it reports no scores.
  # This checks that each extractor forwards to the correct field of the tuned learner.
  task = tsk("sonar")
  learner = lrn("classif.debug", validate = 0.2, early_stopping = TRUE, iter = 10)$train(task)

  at = auto_tuner(
    tuner = tnr("random_search"),
    learner = lrn("classif.debug"),
    resampling = rsmp("holdout"),
    measure = msr("classif.ce"),
    term_evals = 1
  )
  at$state = list(model = list(learner = learner))

  private_at = get_private(at)
  expect_equal(private_at$.extract_internal_valid_scores(), learner$internal_valid_scores)
  expect_equal(private_at$.extract_best_valid_scores(), learner$best_valid_scores)
  expect_true(private_at$.extract_best_valid_scores()$acc >= private_at$.extract_internal_valid_scores()$acc)
})
