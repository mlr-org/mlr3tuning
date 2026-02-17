# soon replaced by import from rush package
skip_if_no_redis = function() {
  testthat::skip_on_cran()

  if (identical(Sys.getenv("RUSH_TEST_USE_REDIS"), "true") && redux::redis_available()) {
    return(invisible())
  }

  testthat::skip("Redis is not available")
}

redis_configuration = function() {
  config = redux::redis_config()
  r = redux::hiredis(config)
  r$FLUSHDB()
  config
}

start_rush = function(n_workers = 2, worker_type = "remote") {
  config = redis_configuration()

  rush::rush_plan(n_workers = n_workers)

  rush = if (packageVersion("rush") <= "0.4.1") {
    rush::rush_plan(n_workers = n_workers, worker_type = worker_type)
    rush::rsh(config = config)
  } else {
    rush::rush_plan(n_workers = n_workers, worker_type = worker_type)
    rush::rsh(config = config)
  }

  if (worker_type == "remote") {
    mirai::daemons(n_workers)
  }

  rush
}

start_rush_worker = function(n_workers = 2) {
  config = redis_configuration()

  network_id = paste(sample(LETTERS, 10), collapse = "")
  rush = if (packageVersion("rush") <= "0.4.1") {
    rush::RushWorker$new(network_id = network_id, config = config, remote = FALSE)
  } else {
    rush::RushWorker$new(network_id = network_id, config = config)
  }

  rush
}

# parses the string returned by rush$worker_script() and starts a processx process
start_script_worker = function(script) {
  script = sub('^Rscript\\s+-e\\s+\\"(.*)\\"$', '\\1', script, perl = TRUE)

  px = processx::process$new("Rscript",
    args = c("-e", script),
    supervise = TRUE,
    stderr = "|", stdout = "|")
  px
}
