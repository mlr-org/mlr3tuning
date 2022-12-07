test_that("tune function works with one measure", {
  learner = lrn("classif.rpart", minsplit =  to_tune(1, 10))
  instance = tune(method = "random_search", task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"),
    measures = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_class(instance, "TuningInstanceSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("tune function works with multiple measures", {
  learner = lrn("classif.rpart", minsplit =  to_tune(1, 10))
  instance = tune(method = "random_search", task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"),
    measures = msrs(c("classif.ce", "classif.acc")), term_evals = 2, batch_size = 1)

  expect_class(instance, "TuningInstanceMultiCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})

test_that("tune function works without measure", {
  learner = lrn("classif.rpart", minsplit =  to_tune(1, 10))
  instance = tune(method = "random_search", task = tsk("pima"),
    learner = learner, resampling = rsmp ("holdout"), term_evals = 2)

  expect_measure(instance$objective$measures[[1]])
})

test_that("tune interface is equal to TuningInstanceSingleCrit", {
  tune_args = formalArgs(tune)
  tune_args = tune_args[tune_args != "method" & tune_args != "..."]
  tune_args[tune_args == "measures"] = "measure"

  instance_args = formalArgs(TuningInstanceSingleCrit$public_methods$initialize)
  instance_args = c(instance_args, "term_evals", "term_time")

  expect_set_equal(tune_args, instance_args)
})

test_that("tune interface is equal to TuningInstanceMultiCrit", {
  tune_args = formalArgs(tune)
  tune_args = tune_args[tune_args != "method" & tune_args != "..."]

  instance_args = formalArgs(TuningInstanceMultiCrit$public_methods$initialize)
  instance_args = c(instance_args, "term_evals", "term_time")

  expect_set_equal(tune_args, instance_args)
})

# evaluate_default ------------------------------------------------------------------

test_that("evaluate_default works", {
  learner = lrn("classif.rpart", cp = to_tune(1e-3, 1))

  instance = tune(
    method = "random_search",
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    evaluate_default = TRUE
  )

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], 0.01)
})

test_that("evaluate_default works with logscale", {
  learner = lrn("classif.rpart", cp = to_tune(1e-3, 1, logscale = TRUE))

  instance = tune(
    method = "random_search",
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    evaluate_default = TRUE
  )

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], log(0.01))
})

test_that("evaluate_default errors with trafo", {
  learner = lrn("classif.rpart", cp = to_tune(p_dbl(-10, 0, trafo = function(x) 10^x)))

  expect_error(tune(
    method = "random_search",
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    evaluate_default = TRUE
  ), "Cannot evaluate default hyperparameter values")
})

test_that("evaluate_default works without transformation and with logscale", {
  learner = lrn("classif.rpart",
    cp = to_tune(1e-3, 1, logscale = TRUE),
    minbucket = to_tune(1, 20))

  instance = tune(
    method = "random_search",
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    evaluate_default = TRUE
  )

  expect_equal(instance$archive$data$x_domain[[1]]$cp, 0.01)
  expect_equal(instance$archive$data$cp[[1]], log(0.01))
  expect_equal(instance$archive$data$x_domain[[1]]$minbucket, 7)
  expect_equal(instance$archive$data$minbucket[[1]], 7)
})

test_that("evaluate_default errors without transformation and with logscale and trafo", {
  learner = lrn("classif.rpart",
    cp = to_tune(1e-3, 1, logscale = TRUE),
    minbucket = to_tune(1, 20),
    minsplit = to_tune(p_int(0, 3, trafo = function(x) 2^x)))

  expect_error(tune(
    method = "random_search",
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 2,
    evaluate_default = TRUE
  ), "Cannot evaluate default hyperparameter values")
})

test_that("evaluate_default errors with extra trafo", {
  learner = lrn("classif.rpart")
  search_space = ps(
    cp = p_dbl(1e-3, 1, logscale = TRUE),
    minbucket = p_int(1, 20),
    minsplit = p_int(1, 20),
    .extra_trafo = function(x, param_set) {
      x$minsplit = 3
      x
    }
  )

  expect_error(tune(
    method = "random_search",
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    search_space = search_space,
    term_evals = 2,
    evaluate_default = TRUE
  ), "Cannot evaluate default hyperparameter values")
})

test_that("evaluate_default errors with old parameter set api", {
  learner = lrn("classif.rpart")
  search_space = ParamSet$new(list(
    ParamDbl$new(id = "cp", lower = -10, upper = 0)
  ))
  search_space$trafo = function(x, param_set) {
    x$cp = 10^(x$cp)
    x
  }

  expect_error(tune(
    method = "random_search",
    task = tsk("iris"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    search_space = search_space,
    term_evals = 2,
    evaluate_default = TRUE
  ), "Cannot evaluate default hyperparameter values")
})

test_that("tune function accepts string input for method", {
  learner = lrn("classif.rpart", minsplit =  to_tune(1, 10))
  instance = tune(method = "random_search", task = tsk("pima"), learner = learner, resampling = rsmp ("holdout"),
    measures = msr("classif.ce"), term_evals = 2, batch_size = 1)

  expect_class(instance, "TuningInstanceSingleCrit")
  expect_data_table(instance$archive$data, nrows = 2)
  expect_class(instance$terminator, "TerminatorEvals")
})
