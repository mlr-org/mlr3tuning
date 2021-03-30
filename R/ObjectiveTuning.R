#' @title ObjectiveTuning
#'
#' @description
#' Stores the objective function that estimates the performance of
#' hyperparameter configurations. This class is usually constructed internally
#' by the [TuningInstanceSingleCrit] / [TuningInstanceMultiCrit].
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#' @template param_allow_retrain
#'
#' @export
ObjectiveTuning = R6Class("ObjectiveTuning",
  inherit = Objective,
  public = list(

    #' @field task ([mlr3::Task]).
    task = NULL,

    #' @field learner ([mlr3::Learner]).
    learner = NULL,

    #' @field resampling ([mlr3::Resampling]).
    resampling = NULL,

    #' @field measures (list of [mlr3::Measure]).
    measures = NULL,

    #' @field store_models (`logical(1)`).
    store_models = NULL,

    #' @field store_benchmark_result (`logical(1)`).
    store_benchmark_result = NULL,

    #' @field archive ([ArchiveTuning]).
    archive = NULL,

    #' @field allow_retrain (`logical(1)`).
    allow_retrain = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measures, check_values = TRUE, store_benchmark_result = TRUE,
      store_models = FALSE, allow_retrain = FALSE) {

      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE))
      self$resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)
      self$store_benchmark_result = assert_flag(store_benchmark_result)
      self$store_models = assert_flag(store_models)
      self$allow_retrain = assert_flag(allow_retrain)
      if (!resampling$is_instantiated) {
        self$resampling$instantiate(self$task)
      }

      codomain = ParamSet$new(map(self$measures, function(s) {
        ParamDbl$new(id = s$id, tags = ifelse(s$minimize, "minimize", "maximize"))
      }))

      super$initialize(id = sprintf("%s_on_%s", self$learner$id, self$task$id), domain = self$learner$param_set,
        codomain = codomain, check_values = check_values)
    }
  ),

  private = list(
    .eval_many = function(xss) {
      learners = map(xss, function(x) {
        learner = self$learner$clone(deep = TRUE)
        learner$param_set$values = insert_named(learner$param_set$values, x)
        return(learner)
      })

      # automatic matching of retrainable learners
      # for every learner
      #  - keep retrainable resample results
      #  - find resample result that can be retrained most efficiently
      #  - retrain or if no retrain is possible, resample
      if (self$allow_retrain) {
        rrs = self$archive$benchmark_result$resample_results$resample_result
        res = map(learners, function(learner) {
          rrrs = keep(rrs, function(rr) rr$is_retrainable(learner$param_set$values))
          if (length(rrrs) > 0) {
            retrain_values = map(rrrs, function(rrr) rrr$learner$param_set$get_values(tags = "retrain"))
            rrrs[[learner$which_retrain(retrain_values)]]$clone()$retrain(learner$param_set$values)
          } else {
            resample(self$task, learner, self$resampling, store_models = TRUE)
          }
        })
        bmr = do.call("c", res)
      } else {
        design = benchmark_grid(self$task, learners, self$resampling)
        bmr = benchmark(design, store_models = self$store_models)
      }

      aggr = bmr$aggregate(self$measures)
      y = map_chr(self$measures, "id")

      if (self$store_benchmark_result) {
        if (is.null(self$archive$benchmark_result)) {
          self$archive$benchmark_result = bmr
        } else {
          self$archive$benchmark_result$combine(bmr)
        }
        cbind(aggr[, y, with = FALSE], uhash = bmr$uhashes)
      } else {
        aggr[, y, with = FALSE]
      }
    }
  ),

  active = list(

    #' @field continue_hash (`character()`)\cr
    #' Stores the `uhash` of models that should be continued in the next
    #' `$eval_many` call. Returns `NULL` if the `Learner` does not support
    #' `$continue()`, the learner is a `GraphLearner` or `store_models = FALSE`.
    continue_hash = function(rhs) {
      if(missing(rhs)) {
        if("continue" %nin% self$learner$properties
          || !self$store_models
          || "GraphLearner" %in% class(self$learner)) {
            NULL
        } else {
          private$.continue_hash
        }
      } else {
        private$.continue_hash = rhs
      }
    }
  )
)
