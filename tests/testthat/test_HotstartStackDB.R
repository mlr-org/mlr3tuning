test_that("HotstartStack hotstart forward works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(2, 1))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_2$state$model)

   # no adaptable learner
  learner_1 = lrn("classif.rpart")
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStackDB$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), NA_real_)
  expect_null(get_private(hot)$.start_learner(learner, task$hash))

  # equal cost
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 1))

  # one higher, one lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, NA_real_))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_1$state$model)

  # equal learner
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  hot = HotstartStackDB$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_1$state$model)

  # only higher
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 1)
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(NA_real_, NA_real_))
  expect_null(get_private(hot)$.start_learner(learner, task$hash))

  # mixed
  learner_1 = lrn("classif.debug", iter = 1) # lower
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2) # equal
  learner_2$train(task)
  learner_3 = lrn("classif.debug", iter = 3) # higher
  learner_3$train(task)
  learner_4 = lrn("classif.rpart") # different learner
  learner_4$train(task)

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStackDB$new(list(learner_1, learner_2, learner_3, learner_4))

  expect_equal(hot$start_cost(learner, task$hash), c(1, -1, NA_real_, NA_real_))

  # target learner is not able to hotstart
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.rpart")
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(NA_real_, NA_real_))
  expect_null(get_private(hot)$.start_learner(learner, task$hash))
})

test_that("HotstartStack hotstart backwards works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStackDB$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), 0)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_1$state$model)

  # equal cost
  learner_1 = lrn("classif.debug", iter = 4)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 4)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, 0))

  # one higher, one lower
  learner_1 = lrn("classif.debug", iter = 3)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 1)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, NA_real_))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_1$state$model)

  # equal learner
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStackDB$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_1$state$model)

  # only lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStackDB$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), NA_real_)
  expect_null(get_private(hot)$.start_learner(learner, task$hash))

  # mixed
  learner_1 = lrn("classif.debug", iter = 1) # lower
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2) # equal
  learner_2$train(task)
  learner_3 = lrn("classif.debug", iter = 3) # higher
  learner_3$train(task)
  learner_4 = lrn("classif.rpart") # different learner
  learner_4$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties[learner$properties %in% "hotstart_forward"] = "hotstart_backward"
  hot = HotstartStackDB$new(list(learner_1, learner_2, learner_3, learner_4))

  expect_equal(hot$start_cost(learner, task$hash), c(NA_real_, -1, 0, NA_real_))
})

test_that("HotstartStack hotstart forward and backwards works", {
  task = tsk("pima")

  # default
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 0))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_2$state$model)

  # equal cost
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 3)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 1))

  # one higher, one lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(1, 0))
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_2$state$model)

  # equal learner
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStackDB$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), -1)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_1$state$model)

  # only higher
  learner_1 = lrn("classif.debug", iter = 2)
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 3)
  learner_2$train(task)

  learner = lrn("classif.debug", iter = 1)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStackDB$new(list(learner_1, learner_2))

  expect_equal(hot$start_cost(learner, task$hash), c(0, 0))

  # only lower
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStackDB$new(list(learner_1))

  expect_equal(hot$start_cost(learner, task$hash), 1)
  expect_equal(get_private(hot)$.start_learner(learner, task$hash)$state$model, learner_1$state$model)

  # mixed
  learner_1 = lrn("classif.debug", iter = 1) # lower
  learner_1$train(task)
  learner_2 = lrn("classif.debug", iter = 2) # equal
  learner_2$train(task)
  learner_3 = lrn("classif.debug", iter = 3) # higher
  learner_3$train(task)
  learner_4 = lrn("classif.rpart") # different learner
  learner_4$train(task)

  learner = lrn("classif.debug", iter = 2)
  learner$properties =  c(learner$properties, "hotstart_backward")
  hot = HotstartStackDB$new(list(learner_1, learner_2, learner_3, learner_4))

  expect_equal(hot$start_cost(learner, task$hash), c(1, -1, 0, NA_real_))
})

test_that("HotstartStackDB add method works", {
  task = tsk("pima")

  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  hot = HotstartStackDB$new(list(learner_1))

  learner_2 = lrn("classif.debug", iter = 2)
  learner_2$train(task)
  hot$add(list(learner_2))

  stack = DBI::dbGetQuery(hot$connection, "SELECT * FROM stack")

  expect_data_frame(stack, nrows = 2, ncols = 4)
  expect_equal(stack$hotstart_value[[1]], 1)
  expect_equal(stack$hotstart_value[[2]], 2)

  # no hotstart value
  learner_1 = lrn("classif.debug", x = 0.5)
  learner_1$train(task)
  learner_1$state$param_vals$iter = NULL # iter set by default. Assume it is not.
  hot = HotstartStackDB$new(list(learner_1))
  stack = DBI::dbGetQuery(hot$connection, "SELECT * FROM stack")

  expect_equal(stack$hotstart_value[[1]], NA_real_)
})

test_that("copy state works", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  learner_2 = lrn("classif.debug", iter = 2)
  hot = HotstartStackDB$new(learner_1)
  learner_2$hotstart_stack = hot

  start_learner = get_private(learner_2$hotstart_stack)$.start_learner(learner_2, task$hash)
  start_learner$param_set$values$iter = 2

  expect_equal(learner_1$param_set$values$iter, 1)
  expect_equal(start_learner$param_set$values$iter, 2)
})

test_that("learners are hotstarted from data base when resample is used", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  rr = resample(task, learner_1, resampling, store_models = TRUE)

  learner = lrn("classif.debug", iter = 2)
  hot = HotstartStackDB$new(rr$learners)
  learner$hotstart_stack = hot

  rr_2 = resample(task, learner, resampling, store_models = TRUE, allow_hotstart = TRUE)
  pmap(list(rr$learners, rr_2$learners), function(l1, l2) {
    expect_equal(l2$param_set$values$iter, 2)
    expect_class(l2$model, "classif.debug_model")
    expect_equal(l2$model$iter, 2)
    expect_equal(l1$model$id, l2$model$id)
  })
})

test_that("learners are hotstarted from data base when benchmark is used", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_2 = lrn("classif.debug", iter = 2)
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  design = benchmark_grid(task, list(learner_1, learner_2), resampling)
  bmr = benchmark(design, store_models = TRUE)

  learners = unlist(map(seq_len(bmr$n_resample_results), function(i) bmr$resample_result(i)$learners))
  hot = HotstartStackDB$new(learners)
  ids = map_chr(learners, function(l) l$model$id)

  learner = lrn("classif.debug", iter = 3)
  learner$hotstart_stack = hot

  design = benchmark_grid(task, learner, resampling)
  bmr_2 = benchmark(design, store_models = TRUE, allow_hotstart = TRUE)

  map(bmr_2$resample_result(1)$learners, function(l1) {
    expect_equal(l1$param_set$values$iter, 3)
    expect_class(l1$model, "classif.debug_model")
    expect_equal(l1$model$iter, 3)
    expect_true(l1$model$id %in% ids)
    expect_null(l1$hotstart_stack)
  })
})

test_that("learners are trained and hotstarted from data base when benchmark is called", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_2 = lrn("classif.debug", iter = 2)
  resampling = rsmp("cv", folds = 3)
  resampling$instantiate(task)

  design = benchmark_grid(task, list(learner_1, learner_2), resampling)
  bmr = benchmark(design, store_models = TRUE)

  learners = unlist(map(seq_len(bmr$n_resample_results), function(i) bmr$resample_result(i)$learners))
  hot = HotstartStackDB$new(learners)
  ids = map_chr(learners, function(l) l$model$id)

  learner_3 = lrn("classif.debug", iter = 4)
  learner_3$hotstart_stack = hot
  learner_4 = lrn("classif.rpart")
  learner_4$hotstart_stack = hot

  design = benchmark_grid(task, list(learner_3, learner_4), resampling)
  bmr_2 = benchmark(design, store_models = TRUE, allow_hotstart = TRUE)

  map(bmr_2$resample_result(1)$learners, function(l1) {
    expect_equal(l1$param_set$values$iter, 4)
    expect_class(l1$model, "classif.debug_model")
    expect_equal(l1$model$iter, 4)
    expect_true(l1$model$id %in% ids[4:6])
    expect_null(l1$hotstart_stack)
  })

  map(bmr_2$resample_result(2)$learners, function(l1) {
    expect_class(l1$model, "rpart")
    expect_null(l1$hotstart_stack)
  })
})

test_that("learners are cloned when hotstarting from data base is applied", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  resampling = rsmp("holdout")
  resampling$instantiate(task)

  design = benchmark_grid(task, learner_1, resampling)
  bmr  = benchmark(design, store_models = TRUE, allow_hotstart = TRUE)

  learner_2 = lrn("classif.debug", iter = 2)
  hot = HotstartStackDB$new(bmr$resample_result(1)$learners)
  learner_2$hotstart_stack = hot

  design = benchmark_grid(task, learner_2, resampling)
  bmr  = benchmark(design, store_models = TRUE, allow_hotstart = TRUE)

  expect_equal(bmr$resample_result(1)$learners[[1]]$param_set$values$iter, 2)
  expect_equal(bmr$resample_result(1)$learners[[1]]$model$iter, 2)

  stack = DBI::dbGetQuery(hot$connection, "SELECT * FROM stack")
  expect_equal(stack$hotstart_value, 1)
  expect_equal(unserialize(stack$state[[1]])$model$iter, 1)
  expect_equal(bmr$resample_result(1)$learners[[1]]$model$id, unserialize(stack$state[[1]])$model$id)
})

test_that("learner limit works", {
  task = tsk("pima")
  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)
  learners = replicate(100, learner_1)

  hot = HotstartStackDB$new(learners)
  expect_equal(get_private(hot)$.learner_count, 100)

  hot$add(learner_1)
  expect_equal(get_private(hot)$.learner_count, 101)

  hot$add(learners)
  expect_equal(get_private(hot)$.learner_count, 201)

  hot$learner_limit = 100
  hot$add(learner_1)
  expect_equal(get_private(hot)$.learner_count, 51)
})

test_that("HotstartStack db file is removed by garbage collection", {
  task = tsk("pima")

  learner_1 = lrn("classif.debug", iter = 1)
  learner_1$train(task)

  hot = HotstartStackDB$new(list(learner_1))
  file = hot$stack

  expect_file_exists(file)

  rm(hot)
  gc()

  expect_true(!file.exists(file))
})
