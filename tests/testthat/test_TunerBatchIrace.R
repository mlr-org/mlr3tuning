skip_if_not_installed("irace")

test_that("TunerIrace", {
  x = capture.output({z = test_tuner("irace", term_evals = 42, real_evals = 39)})
  instance = z$inst
  archive = instance$archive$data
  tuner = z$tuner

  # check archive columns
  expect_subset(c("race", "step", "configuration", "instance"), names(archive))

  # check optimization direction
  # first elite of the first race should have the lowest average performance
  load(tuner$param_set$values$logFile)
  elites = iraceResults$allElites
  aggr = archive[race == 1, .(classif.ce = mean(classif.ce)), by = configuration]
  expect_equal(aggr[which.min(classif.ce), configuration], elites[[1]][1])

  # the performance of the best configuration should be the mean performance across all evaluated instances
  configuration_id = instance$result$configuration
  expect_equal(unname(instance$result_y), mean(archive[configuration == configuration_id, classif.ce]))
})

test_that("TunerIrace works with dependencies", {
  search_space = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_int(lower = 1, upper = 10, depends = cp == 0.005)
  )
  instance = TuningInstanceBatchSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
    trm("evals", n_evals = 96), search_space)
  tuner = tnr("irace")
  x = capture.output({tuner$optimize(instance)})

  archive = instance$archive$data
  expect_true(all(is.na(archive[cp != 0.005, minsplit])))
  expect_double(archive$cp)
})

test_that("TunerIrace works with logical parameters", {
  search_space = ps(keep_model = p_lgl())
  instance = TuningInstanceBatchSingleCrit$new(tsk("mtcars"), lrn("regr.rpart"), rsmp("holdout"), msr("regr.mse"),
    trm("evals", n_evals = 42), search_space)
  tuner = tnr("irace")
  x = capture.output({tuner$optimize(instance)})
  expect_logical(instance$archive$best()$keep_model)
})

test_that("TunerIrace uses digits", {
  search_space = ps(cp = p_dbl(lower = pi * 1e-20, upper = 5.242e12 / 1e13))
  instance = TuningInstanceBatchSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
  trm("evals", n_evals = 30), search_space)
  tuner = tnr("irace", nbIterations = 1L, minNbSurvival = 1)
  x = capture.output({expect_data_table(tuner$optimize(instance))})
})

test_that("TunerIrace works with unnamed discrete values", {
  # we had a bug here, see (mlr) issue #627
  search_space = ps(minsplit = p_int(lower = 2L, upper = 7L))
  inst = TuningInstanceBatchSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
    trm("evals", n_evals = 50), search_space)
  tuner = tnr("irace")
  x = capture.output({expect_data_table(tuner$optimize(inst))})
})
