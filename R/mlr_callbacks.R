#' @title Backup Benchmark Result Callback
#'
#' @include CallbackAsyncTuning.R CallbackBatchTuning.R
#' @name mlr3tuning.backup
#'
#' @description
#' This [mlr3misc::Callback] writes the [mlr3::BenchmarkResult] after each batch to disk.
#'
#' @examples
#' clbk("mlr3tuning.backup", path = "backup.rds")
#'
#' # tune classification tree on the pima data set
#' instance = tune(
#'   tuner = tnr("random_search", batch_size = 2),
#'   task = tsk("pima"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   term_evals = 4,
#'   callbacks = clbk("mlr3tuning.backup", path = tempfile(fileext = ".rds"))
#' )
NULL

load_callback_backup = function() {
  callback_batch_tuning("mlr3tuning.backup",
    label = "Backup Benchmark Result Callback",
    man = "mlr3tuning::mlr3tuning.backup",
    on_optimization_begin = function(callback, context) {
      if (is.null(callback$state$path)) callback$state$path = "bmr.rds"
      assert_path_for_output(callback$state$path)
    },

    on_optimizer_after_eval = function(callback, context) {
      if (file.exists(callback$state$path)) unlink(callback$state$path)
      saveRDS(context$instance$archive$benchmark_result, callback$state$path)
    }
  )
}

#' @title Measure Callback
#'
#' @include CallbackAsyncTuning.R CallbackBatchTuning.R
#' @name mlr3tuning.measures
#' @aliases mlr3tuning.async_measures
#'
#' @description
#' This [mlr3misc::Callback] scores the hyperparameter configurations on additional measures while tuning.
#' Usually, the configurations can be scored on additional measures after tuning (see [ArchiveBatchTuning]).
#' However, if the memory is not sufficient to store the [mlr3::BenchmarkResult], it is necessary to score the additional measures while tuning.
#' The measures are not taken into account by the tuner.
#'
#' @examples
#' clbk("mlr3tuning.measures")
#'
#' # additionally score the configurations on the accuracy measure
#' instance = tune(
#'   tuner = tnr("random_search", batch_size = 2),
#'   task = tsk("pima"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   term_evals = 4,
#'   callbacks = clbk("mlr3tuning.measures", measures = msr("classif.acc"))
#' )
#'
NULL

load_callback_measures = function() {
  callback_batch_tuning("mlr3tuning.measures",
    label = "Additional Measures Callback",
    man = "mlr3tuning::mlr3tuning.measures",
    on_optimization_begin = function(callback, context) {
      callback$state$measures = assert_measures(as_measures(callback$state$measures, clone = TRUE))
      callback$state$ids = map_chr(callback$state$measures, "id")
      assert_names(callback$state$ids, type = "unique", .var.name = "measures")
      if (any(callback$state$ids %in% map_chr(context$instance$objective$measures, "id"))) {
        stopf("The measure id(s) '%s' are already used by the instance. Please pass the measures with a different id.", as_short_string(callback$state$ids))
      }
    },

    on_eval_before_archive = function(callback, context) {
      set(context$aggregated_performance, j = callback$state$ids, value = context$benchmark_result$aggregate(callback$state$measures)[, callback$state$ids, with = FALSE])
    }
  )
}

load_callback_async_measures = function() {
  callback_async_tuning("mlr3tuning.async_measures",
    label = "Additional Rush Measures Callback",
    man = "mlr3tuning::mlr3tuning.measures",

    on_optimization_begin = function(callback, context) {
      assert_measures(callback$state$measures)
      ids = map_chr(callback$state$measures, "id")
      assert_names(ids, type = "unique", .var.name = "measures")
      if (any(ids %in% map_chr(context$instance$objective$measures, "id"))) {
        stopf("The measure id(s) '%s' are already used by the instance. Please pass the measures with a different id.", as_short_string(ids))
      }
    },

    on_eval_before_archive = function(callback, context) {
      ids = map_chr(callback$state$measures, "id")
      scores = as.list(context$resample_result$aggregate(callback$state$measures))
      context$aggregated_performance = c(context$aggregated_performance, scores)
    }
  )
}

#' @title MLflow Connector Callback
#'
#' @include CallbackAsyncTuning.R CallbackBatchTuning.R
#' @name mlr3tuning.asnyc_mlflow
#'
#' @description
#' This [mlr3misc::Callback] logs the hyperparameter configurations and the performance of the configurations to MLflow.
#'
#' @examplesIf requireNamespace("mlflow")
#' @examples
#' clbk("mlr3tuning.async_mlflow", tracking_uri = "http://localhost:5000")
#'
#' \dontrun{
#' rush::rush_plan(n_workers = 4)
#'
#' learner = lrn("classif.rpart",
#'   minsplit = to_tune(2, 128),
#'   cp = to_tune(1e-04, 1e-1))
#'
#' instance = TuningInstanceAsyncSingleCrit$new(
#'   task = tsk("pima"),
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 20),
#'   store_benchmark_result = FALSE,
#'   callbacks = clbk("mlr3tuning.rush_mlflow", tracking_uri = "http://localhost:8080")
#' )
#'
#' tuner = tnr("random_search_v2")
#' tuner$optimize(instance)
#' }
NULL

# we need this as a callback, because we cannot overwrite the private$.assign_result method from the Tuner,
# because for TunerFromOptimizer this method is never called
load_callback_internal_tuning = function(batch) {

  on_result = function(callback, context) {
    inst = context$instance
    internal_tuned_values = inst$archive$best()[, "internal_tuned_values", with = FALSE]$internal_tuned_values

    # right now, this contains the values tuned by the optimizer and the values that were set in the learner
    learner_param_vals = context$result$learner_param_vals
    # we now have to:
    # * add the internally optimized values
    # * disable the internal tuning for the internally optimized values

    learner = inst$objective$learner
    new_learner_param_vals = pmap(list(itv = internal_tuned_values, lpv = learner_param_vals), function(itv, lpv) {
      prev_pvs = learner$param_set$values

      learner$param_set$values = insert_named(lpv, itv)
      on.exit({
        inst$objective$learner$param_set$values = prev_pvs
      })

      learner$param_set$disable_internal_tuning(context$instance$internal_search_space$ids())
      nlpv = inst$objective$learner$param_set$values
      on.exit()

      nlpv
    })
    context$result[, let(learner_param_vals = new_learner_param_vals, internal_tuned_values = internal_tuned_values)]
  }

  if (batch) {
    callback_batch_tuning("mlr3tuning.batch_internal_tuning",
      man = "mlr3tuning::internal_tuning",
      on_eval_before_archive = function(callback, context) {
        new_uhashes = tail(context$benchmark_result$uhashes, n = nrow(context$aggregated_performance))

        internal_tuned_values_aggr = map(new_uhashes, function(uhash) {
          states = get_private(context$benchmark_result)$.data$learner_states(uhash)

          internal_tuned_values = transpose_list(map(states, "internal_tuned_values"))
          context$instance$internal_search_space$aggr_internal_tuned_values(internal_tuned_values)
        })
        context$aggregated_performance[, let(internal_tuned_values = internal_tuned_values_aggr)]
      },
      on_result = on_result
    )
  } else {
    callback_async_tuning("mlr3tuning.async_internal_tuning",
      on_eval_before_archive = function(callback, context) {
        states = get_private(context$resample_result)$.data$learner_states()
        itvs = transpose_list(map(states, "internal_tuned_values"))
        internal_tuned_values_aggr = context$instance$internal_search_space$aggr_internal_tuned_values(itvs)
        # here, aggregated_performance is a list not a data.table
        context$aggregated_performance$internal_tuned_values = list(internal_tuned_values_aggr)
      },
      on_result = on_result
    )
  }
}

load_callback_async_mlflow = function() {
  callback_async_tuning("mlr3tuning.async_mlflow",
    label = "MLflow Connector",
    man = "mlr3tuning::mlr3tuning.async_mlflow",

    on_optimization_begin = function(callback, context) {
      require_namespaces("mlflow")

      # create mlflow client
      callback$state$client = mlflow::mlflow_client(callback$state$tracking_uri)

      # setup experiment
      name = sprintf("%s_%s_%s", context$optimizer$id, context$instance$objective$id, context$instance$rush$network_id)
      callback$state$experiment_id = mlflow::mlflow_create_experiment(
        name = name,
        client = callback$state$client)

      callback$state$measure_ids = context$instance$archive$cols_y
    },

    on_eval_after_xs = function(callback, context) {
      require_namespaces("mlflow")

      client = callback$state$client
      experiment_id = callback$state$experiment_id

      # start run
      callback$state$run_uuid = mlflow::mlflow_start_run(
        experiment_id = experiment_id,
        client = client)$run_uuid

      iwalk(context$xs, function(value, id) {
        mlflow::mlflow_log_param(
          key = id,
          value = value,
          run = callback$state$run_uuid,
          client = client)
      })
    },

    on_eval_before_archive = function(callback, context) {
      client = callback$state$client
      run_uuid = callback$state$run_uuid

      walk(callback$state$measure_ids, function(id) {
        mlflow::mlflow_log_metric(
          key = id,
          value = context$aggregated_performance[[id]],
          run = run_uuid,
          client = client)
      })

      mlflow::mlflow_end_run(
        status = "FINISHED",
        run_id = run_uuid,
        client = client
      )
    }
  )
}

#' @title Default Configuration Callback
#'
#' @include CallbackAsyncTuning.R CallbackBatchTuning.R
#' @name mlr3tuning.async_default_configuration
#'
#' @description
#' These [CallbackAsyncTuning] and [CallbackBatchTuning] evaluate the default hyperparameter values of a learner.
NULL

load_callback_async_default_configuration = function() {
  callback_async_tuning("mlr3tuning.async_default_configuration",
    label = "Default Configuration",
    man = "mlr3tuning::mlr3tuning.default_configuration",

    on_optimization_begin = function(callback, context) {
      instance = context$instance
      # values are on the learner scale i.e. possible transformation are already applied
      xs = default_values(instance$objective$learner, instance$search_space, instance$objective$task)

      # parameters with exp transformation and log inverse transformation
      # parameters with unknown inverse transformation
      # parameter set with trafo
      if ("set_id" %in% names(ps())) {
        # old paradox
        has_logscale = map_lgl(instance$search_space$params, function(param) get_private(param)$.has_logscale)

        has_trafo = map_lgl(instance$search_space$params, function(param) get_private(param)$.has_trafo)

        has_extra_trafo = get_private(instance$search_space)$.has_extra_trafo
      } else {
        has_logscale = map_lgl(instance$search_space$params$.trafo, function(x) identical(x, exp))

        has_trafo = map_lgl(instance$search_space$params$.trafo, function(x) !is.null(x) && !identical(x, exp))

        has_extra_trafo = !is.null(instance$search_space$extra_trafo)
      }

      if (any(has_trafo) || has_extra_trafo) {
        stop("Cannot evaluate default hyperparameter values. Search space contains transformation functions with unknown inverse function.")
      }

      # inverse parameter with exp transformation
      xs = map_if(xs, has_logscale, log)

      context$instance$archive$push_points(list(xs))
    }
  )
}

load_callback_default_configuration = function() {
  callback_batch_tuning("mlr3tuning.default_configuration",
    label = "Default Configuration",
    man = "mlr3tuning::mlr3tuning.default_configuration",

    on_optimization_begin = function(callback, context) {
      instance = context$instance
      # values are on the learner scale i.e. possible transformation are already applied
      xs = default_values(instance$objective$learner, instance$search_space, instance$objective$task)

      # parameters with exp transformation and log inverse transformation
      # parameters with unknown inverse transformation
      # parameter set with trafo
      if ("set_id" %in% names(ps())) {
        # old paradox
        has_logscale = map_lgl(instance$search_space$params, function(param) get_private(param)$.has_logscale)

        has_trafo = map_lgl(instance$search_space$params, function(param) get_private(param)$.has_trafo)

        has_extra_trafo = get_private(instance$search_space)$.has_extra_trafo
      } else {
        has_logscale = map_lgl(instance$search_space$params$.trafo, function(x) identical(x, exp))

        has_trafo = map_lgl(instance$search_space$params$.trafo, function(x) !is.null(x) && !identical(x, exp))

        has_extra_trafo = !is.null(instance$search_space$extra_trafo)
      }

      if (any(has_trafo) || has_extra_trafo) {
        stop("Cannot evaluate default hyperparameter values. Search space contains transformation functions with unknown inverse function.")
      }

      # inverse parameter with exp transformation
      xdt = as.data.table(map_if(xs, has_logscale, log))

      context$instance$eval_batch(xdt)
    }
  )
}

#' @title Save Logs Callback
#'
#' @include CallbackAsyncTuning.R
#' @name mlr3tuning.async_save_logs
#'
#' @description
#' This [CallbackAsyncTuning] saves the logs of the learners to the archive.
NULL

load_callback_async_save_logs = function() {
  callback_async_tuning("mlr3tuning.async_save_logs",
    label = "Save Logs Callback",
    man = "mlr3tuning::mlr3tuning.async_save_logs",

    on_eval_after_resample = function(callback, context) {
      states = get_private(context$resample_result)$.data$learner_states()
      callback$state$log = map(states, function(state) state$log)
    },

    on_eval_before_archive = function(callback, context) {
      context$aggregated_performance = c(context$aggregated_performance, list(log = list(callback$state$log)))
    }
  )
}
