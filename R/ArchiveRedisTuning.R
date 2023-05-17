#' @title Logging object for objective function evaluations
#'
#' @template param_codomain
#' @template param_search_space
#'
#' @export
ArchiveRedisTuning = R6::R6Class("ArchiveRedisTuning",
  inherit = Archive,
  public = list(

    #' @field config ([redux::redis_config])\cr
    #' Configuration for connecting to Redis.
    config = NULL,

    #' @field n_workers ([integer])\cr
    #' Number of workers.
    n_workers = NULL,

    #' @field instance_id ([character])\cr
    #' Unique identifier of the instance.
    instance_id = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param check_values (`logical(1)`)\cr
    #'   ignored.
    #' @param config ([redux::redis_config])\cr
    #'   Configuration for connecting to Redis.
    #' @param n_workers (`integer(1)`)\cr
    #'  Number of workers.
    #' @param instance_id (`character(1)`)\cr
    #' Unique identifier of the instance.
    initialize = function(search_space, codomain, check_values = TRUE, config = redux::redis_config(), n_workers = future::nbrOfFreeWorkers(), instance_id = uuid::UUIDgenerate()) {
      self$config = assert_class(config, "redis_config")
      self$n_workers = assert_integer(n_workers, min.len = 1)
      self$instance_id = assert_string(instance_id)
      r = self$connector
      r$SET(private$.get_key("n_evals"), 0)
      r$SET(private$.get_key("n_results"), 0)
      r$SET(private$.get_key("terminated"), 0)
      super$initialize(search_space = search_space, codomain = codomain, check_values = check_values)
    },

    #' @description
    #' Write `xdt` and `xss` to redis hashes and add keys to eval queue.
    #' Both `xdt` and `xss` are serialized before being written to redis.
    #'
    #' @param xdt ([data.table::data.table])\cr
    #'   Set of untransformed points and meta information.
    #' @param xss ([list])\cr
    #'   List of transformed points.
    write_xdt_xss = function(xdt, xss) {
      assert_data_table(xdt, min.rows = 1L)
      r = self$connector
      keys = uuid::UUIDgenerate(n = nrow(xdt))
      # serialize R objects
      bin_xdt = map(transpose_list(xdt), function(xs) redux::object_to_bin(xs))
      bin_xss = map(xss, function(xs) redux::object_to_bin(xs))
      # add each point to its own hash
      field_xdt = private$.get_key("xdt")
      field_xss = private$.get_key("xss")
      r$pipeline(.commands = pmap(list(keys, bin_xdt), function(key, bin_x) redux::redis$HSET(key, field_xdt, bin_x)))
      r$pipeline(.commands = pmap(list(keys, bin_xss), function(key, bin_xs) redux::redis$HSET(key, field_xss, bin_xs)))
      # add points to queue
      r$command(c("LPUSH", private$.get_key("queue_eval"), keys))
      # add points to set of all points
      r$command(c("SADD", private$.get_key("keys"), keys))
      # increment evals counter
      r$INCRBY(private$.get_key("n_evals"), length(xss))

      invisible(keys)
    },

    #' @description
    #' Pop xs from eval queue and deserialize it.
    #'
    #' @param timeout ([integer])\cr
    #' Timeout in seconds.
    pop_xs = function(timeout = 1) {
      assert_int(timeout, lower = 0)
      r = self$connector
      key = r$BRPOP(private$.get_key("queue_eval"), timeout)[[2]]
      if (is.null(key)) return(NULL)
      list(key = key, xs = redux::bin_to_object(r$HGET(key, private$.get_key("xss"))))
    },

    #' @description
    #' Write ys to redis hash and add key to result queue.
    #'
    #' @param key (`character(1)`)\cr
    #'    Key of the point.
    #' @param ys (`list`)\cr
    #'    List of results and meta information.
    write_ys = function(key, ys) {
      assert_string(key)
      assert_list(ys, min.len = 1)
      r = self$connector
      bin_ys = redux::object_to_bin(ys)
      r$HSET(key, private$.get_key("ys"), bin_ys)
      r$LPUSH(private$.get_key("queue_result"), key)
      r$INCRBY(private$.get_key("n_results"), 1)
      invisible(NULL)
    },

    #' @description
    #' Sync data from redis to local data table.
    #' Pops all keys from result queue and reads `"xdt"`, `"xss"` and `"ys"` from redis hashes.
    sync_data = function() {
      r = self$connector

      # get keys from result queue
      keys = r$command(list("LPOP", private$.get_key("queue_result"), r$LLEN(private$.get_key("queue_result"))))
      if (!is.null(keys)) {
        lg$info("Receiving %i configuration(s)", length(keys))

        # read data from redis hashes
        field_xdt = private$.get_key("xdt")
        xdt = rbindlist(map(keys, function(key) redux::bin_to_object(r$HGET(key, field_xdt))))
        field_xss = private$.get_key("xss")
        xss = map(keys, function(key) redux::bin_to_object(r$HGET(key, field_xss)))
        field_ys = private$.get_key("ys")
        ydt = rbindlist(map(keys, function(key) redux::bin_to_object(r$HGET(key, field_ys))))

        # bind to local data table
        xydt = cbind(xdt, ydt)
        set(xydt, j = "x_domain", value = xss)
        private$.data = rbindlist(list(private$.data, xydt), fill = TRUE, use.names = TRUE)
        setkeyv(private$.data, self$cols_y)
      }
      invisible(private$.data)
    },

    #' @description
    #' Returns the best scoring evaluation(s).
    #' For single-crit optimization, the solution that minimizes / maximizes the objective function.
    #' For multi-crit optimization, the Pareto set / front.
    #'
    #' @param n_select (`integer(1L)`)\cr
    #'   Amount of points to select. Ignored for multi-crit optimization.
    #'
    #' @return [data.table::data.table()]
    best = function(n_select = 1L) {
      tab = self$data
      assert_int(n_select, lower = 1L, upper = nrow(tab))

      max_to_min = self$codomain$maximization_to_minimization
      if (self$codomain$target_length == 1L) {
        top_n = if (max_to_min == 1L) head else tail
        tab[, top_n(.SD, n_select)]
      } else {
        ymat = t(as.matrix(tab[, self$cols_y, with = FALSE]))
        ymat = max_to_min * ymat
        tab[!is_dominated(ymat)]
      }
    },

    #' @description
    #' Resets the archive.
    reset = function() {
      r = self$connector
      walk(self$all_keys, function(key) r$DEL(key))
      r$DEL(private$.get_key("queue_eval"))
      r$DEL(private$.get_key("queue_result"))
      r$DEL(private$.get_key("keys"))
      r$DEL(private$.get_key("n_evals"))
      r$DEL(private$.get_key("n_results"))
      r$DEL(private$.get_key("terminated"))
      private$.data = data.table()
    }
  ),

  active = list(

    #' @field connector ([redux::hiredis])\cr
    #' Redis connector.
    connector = function() {
      redux::hiredis(self$config)
    },

    #' @field all_keys ([character])\cr
    #' All keys of the archive.
    all_keys = function() {
      r = self$connector
      r$SMEMBERS(private$.get_key("keys"))
    },

    #' @field data ([data.table::data.table])\cr
    #' Contains all performed [Objective] function calls.
    data = function(rhs) {
      if (missing(rhs)) {
        self$sync_data()
        return(private$.data)
      }
      private$.data = rhs
    },

    #' @field terminated ([logical])\cr
    #' Whether the tuning process has been terminated.
    terminated = function() {
      r = self$connector
      r$GET(private$.get_key("terminated")) == "1"
    },

    #' @field free_workers ([integer])\cr
    #' Number of free workers.
    free_workers = function() {
      r = self$connector
      self$n_workers - as.integer(r$GET(private$.get_key("n_evals"))) +  as.integer(r$GET(private$.get_key("n_results")))
    },

    #' @field n_evals ([integer])\cr
    #' Number of performed evaluations.
    n_evals = function() {
      r = self$connector
      as.integer(r$GET(private$.get_key("n_results"))) %??% 0
    },

    #' @field length_queue_eval ([integer])\cr
    #' Number of evaluations in queue.
    length_queue_eval = function() {
      r = self$connector
      as.integer(r$LLEN(private$.get_key("queue_eval")))  %??% 0
    }
  ),

  private = list(
    .data = data.table(),

    .get_key = function(key) {
      sprintf("%s:%s", self$instance_id, key)
    }
  )
)
