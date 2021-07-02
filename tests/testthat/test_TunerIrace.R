skip_if_not_installed("irace")

test_that("TunerIrace", {
  z = test_tuner("irace", term_evals = 42, real_evals = 39)
  a = z$inst$archive$data
  tuner = z$tuner

  # check archive columns
  expect_subset(c("race", "step", "configuration", "instance"), names(a))

  # check optimization direction
  # first elite of the first race should have the lowest average performance
  load(tuner$param_set$values$logFile)
  elites = iraceResults$allElites
  aggr = instance$archive$data[race == 1, .(y = mean(y)), by = configuration]
  expect_equal(aggr[which.min(y), configuration], elites[[1]][1])

  # check for mean performance
  expect_equal(z$inst$result$classif.ce, mean(a[id_configuration == z$inst$result$id_configuration,]$classif.ce))
})

test_that("TunerIrace works with dependencies", {
  search_space = ps(
    cp = p_dbl(lower = 0.001, upper = 0.1),
    minsplit = p_int(lower = 1, upper = 10, depends = cp == 0.005)
  )
  instance = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
    trm("evals", n_evals = 96), search_space)
  tuner = tnr("irace")
  tuner$optimize(instance)
  
  a = instance$archive$data
  expect_true(all(is.na(a[cp != 0.005, minsplit])))
  expect_double(a$cp)
})

test_that("TunerIrace works with logical parameters", {
  search_space = ps(keep_model = p_lgl())
  instance = TuningInstanceSingleCrit$new(tsk("mtcars"), lrn("regr.rpart"), rsmp("holdout"), msr("regr.mse"),
    trm("evals", n_evals = 42), search_space)
  tuner = tnr("irace")
  tuner$optimize(instance)
  expect_logical(instance$archive$best()$keep_model)
})

test_that("TunerIrace uses digits", {
  search_space = ps(cp = p_dbl(lower = pi * 1e-20, upper = 5.242e12 / 1e13))
  instance = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"), 
  trm("evals", n_evals = 30), search_space)
  tuner = tnr("irace", nbIterations = 1L, minNbSurvival = 1)
  expect_data_table(tuner$optimize(instance))
})

test_that("TunerIrace works with unnamed discrete values", {
  # we had a bug here, see (mlr) issue #627
  search_space = ps(minsplit = p_int(lower = 2L, upper = 7L))
  inst = TuningInstanceSingleCrit$new(tsk("iris"), lrn("classif.rpart"), rsmp("holdout"), msr("classif.ce"),
    trm("evals", n_evals = 50), search_space)
  tuner = tnr("irace")
  expect_data_table(tuner$optimize(inst))
})
