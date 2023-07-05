
test_that("ArchiveRedis", {
  skip_if(TRUE)

  on.exit({
    archive$reset()
  })

  archive = ArchiveRedisTuning$new(
    search_space = ps(),
    codomain = ps(y = p_dbl(tags = "minimize")),
    config = redux::redis_config(),
    instance_id = "test")
  size = 36
  xdt = data.table(x1 = runif(size), x2 = runif(size))
  xss = transpose_list(xdt)
  ydt = data.table(y = sample(seq(size), size))

  expect_string(archive$instance_id)
  expect_class(archive$config, "redis_config")
  expect_class(archive$connector, "redis_api")
  expect_equal(get_private(archive)$.get_key("x"), "test:x")
  expect_equal(archive$n_workers, 1L)
  r = archive$connector

  keys = archive$write_xdt_xss(xdt, xss)
  expect_character(keys, n.chars = size)
  expect_equal(r$LLEN("test:queue_eval"), size)
  walk(keys, function(key) expect_equal(r$EXISTS(key), 1))
  walk(keys, function(key) expect_equal(r$HEXISTS(key, "test:xdt"), 1))
  walk(keys, function(key) expect_equal(r$HEXISTS(key, "test:xss"), 1))

  xs = archive$pop_xs()
  expect_list(xs, len = 2)
  expect_names(names(xs), identical.to = c("key", "xs"))
  expect_equal(xs$key, keys[1])
  expect_names(names(xs$xs), identical.to = c("x1", "x2"))

  archive$write_ys(keys[1], as.list(ydt[1, ]))
  expect_equal(r$LLEN("test:queue_result"), 1)
  expect_equal(r$EXISTS(keys[1]), 1)

  archive$write_ys(keys[2], as.list(ydt[2, ]))
  archive$write_ys(keys[3], as.list(ydt[3, ]))
  data = archive$data
  expect_data_table(data, nrows = 3, key = "y")

  best = archive$best()
  expect_data_table(best, nrows = 1)
  expect_equal(best$y, min(data$y))
  expect_list(archive$all_keys, types = "character", len = 36)

  archive$reset()
  walk(keys, function(key) expect_equal(r$EXISTS(key), 0))
  expect_equal(r$EXISTS("test:queue_eval"), 0)
  expect_equal(r$EXISTS("test:queue_result"), 0)
})

test_that("Async Random Search", {
  skip_if(TRUE)

  learner = lrn("classif.debug",
    x  = to_tune(),
    sleep_train = function() 0.1
  )

  instance = ti(
    task = tsk("sonar"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    terminator = trm("evals", n_evals = 10),
    check_values = FALSE
  )

  tuner = tnr("async_random_search")

  future::plan("multisession", workers = 2L)

  tuner$optimize(instance)
})

test_that("async callbacks work", {
  
})
