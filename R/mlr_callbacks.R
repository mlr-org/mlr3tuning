#' @title Backup Benchmark Result Callback
#'
#' @include CallbackTuning.R
#' @name mlr3tuning.backup
#'
#' @description
#' This [CallbackTuning] writes the [mlr3::BenchmarkResult] after each batch to disk.
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
  callback_tuning("mlr3tuning.backup",
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
#' @include CallbackTuning.R
#' @name mlr3tuning.measures
#'
#' @description
#' This [CallbackTuning] scores the hyperparameter configurations on additional measures while tuning.
#' Usually, the configurations can be scored on additional measures after tuning (see [ArchiveTuning]).
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
#' # score the configurations on the holdout set
#' task = tsk("pima")
#' splits = partition(task, ratio = 0.8)
#' task$row_roles$use = splits$train
#' task$row_roles$holdout = splits$test
#'
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#' learner$predict_sets = c("test", "holdout")
#'
#' instance = tune(
#'   tuner = tnr("random_search", batch_size = 2),
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msr("classif.ce"),
#'   term_evals = 4,
#'   callbacks = clbk("mlr3tuning.measures", measures = msr("classif.ce",
#'     predict_sets = "holdout", id = "classif.ce_holdout"))
#' )
NULL

load_callback_measures = function() {
  callback_tuning("mlr3tuning.measures",
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
#' @include CallbackTuning.R
#' @name mlr3tuning.mlflow
#'
#' @description
#' This [CallbackTuning] logs the hyperparameter configurations and the performance of the configurations to MLflow.
#'
#' @examples
#' clbk("mlr3tuning.mlflow", tracking_uri = "http://localhost:5000")
#'
#' \dontrun{
#' rush::rush_plan(n_workers = 4)
#'
#' learner = lrn("classif.rpart",
#'   minsplit  = to_tune(2, 128),
#'   cp        = to_tune(1e-04, 1e-1))
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

load_callback_async_mlflow = function() {
  callback_async_tuning("mlr3tuning.async_mlflow",
    label = "MLflow Connector",
    man = "mlr3tuning::mlr3tuning.mlflow",

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
        client =  client)$run_uuid

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

#' @title Hotstart Callback
#'
#' @include CallbackTuning.R
#' @name mlr3tuning.async_hotstart
#'
#' @description
#' This [CallbackTuning] enables hotstarting for the hyperparameter tuning of an XGBoost learner.
#'
NULL

load_callback_async_hotstart = function() {
  callback_async_tuning("mlr3tuning.async_hotstart",
    label = "Hotstart Callback",
    man = "mlr3tuning::mlr3tuning.hotstart",

    on_optimization_begin = function(callback, context) {
      lg$debug("Starting hotstart callback")
      if (all(c("hotstart_forward", "hotstart_backward") %nin% context$instance$objective$learner$properties)) {
        stopf("Hotstart is not supported by %s", format(context$instance$objective$learner))
      }

      lg$debug("Creating hotstart stack")
      browser()
      context$instance$objective$learner$hotstart_stack = HotstartStack$new()
    },

    on_eval_after_resample = function(callback, context) {
      lg$debug("Adding learner to hotstart stack")
      browser()
      context$objective_tuning$learner$hotstart_stack$add(context$resample_result$learners)
    }
  )
}






