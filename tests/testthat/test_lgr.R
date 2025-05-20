test_that("logger works", {
  on.exit({
    lgr::get_logger("mlr3")$set_threshold(NULL)
    lgr::get_logger("mlr3/core")$set_threshold(NULL)
  })

  lgr::logger_tree()

  devtools::load_all("../mlr3")

  res = capture_output(tune(
    tuner = tnr("random_search"),
    task = tsk("pima"),
    learner = lrn("classif.featureless"),
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.acc"),
    term_evals = 2L
  ))
  expect_match(res, "\\[mlr3\\]")
  expect_match(res, "\\[bbotk\\]")


  lgr::get_logger("mlr3")$set_threshold("error")
  res = capture_output(tune(
    tuner = tnr("random_search"),
    task = tsk("pima"),
    learner = lrn("classif.featureless"),
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.acc"),
    term_evals = 2L
  ))
  expect_equal(res, "")

  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(tune(
    tuner = tnr("random_search"),
    task = tsk("pima"),
    learner = lrn("classif.featureless"),
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.acc"),
    term_evals = 2L
  ))
  expect_match(res, "\\[mlr3\\]")
  expect_match(res, "\\[bbotk\\]")

  lgr::get_logger("mlr3/bbotk")$set_threshold("error")
  res = capture_output(tune(
    tuner = tnr("random_search"),
    task = tsk("pima"),
    learner = lrn("classif.featureless"),
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.acc"),
    term_evals = 2L
  ))
  expect_match(res, "\\[mlr3\\]")
  expect_no_match(res, "\\[bbotk\\]")

  lgr::get_logger("mlr3/bbotk")$set_threshold("info")
  lgr::get_logger("mlr3/core")$set_threshold("error")
  res = capture_output(tune(
    tuner = tnr("random_search"),
    task = tsk("pima"),
    learner = lrn("classif.featureless"),
    resampling = rsmp("cv", folds = 3L),
    measures = msr("classif.acc"),
    term_evals = 2L
  ))
  expect_no_match(res, "\\[mlr3\\]")
  expect_match(res, "\\[bbotk\\]")



  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L)))
  expect_match(res, "\\[mlr3\\]")

  lgr::get_logger("mlr3/core")$set_threshold("error")
  res = capture_output(resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L)))
  expect_equal(res, "")

  # if the child logger is configured, it overrides the parent logger
  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L)))
  expect_equal(res, "")
})

test_that("thresholds are restored on workers", {

  on.exit({
    lgr::get_logger("mlr3")$set_threshold(NULL)
    lgr::get_logger("mlr3/core")$set_threshold(NULL)
  })

  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_match(res, "\\[mlr3\\]")

  lgr::get_logger("mlr3")$set_threshold("error")

  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_equal(res, "")

  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_match(res, "\\[mlr3\\]")

  lgr::get_logger("mlr3/core")$set_threshold("error")
  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_equal(res, "")

  # if the child logger is configured, it overrides the parent logger
  lgr::get_logger("mlr3")$set_threshold("info")
  res = capture_output(with_future(future::multisession, {
    resample(tsk("pima"), lrn("classif.featureless"), rsmp("cv", folds = 3L))
  }))
  expect_equal(res, "")
})

