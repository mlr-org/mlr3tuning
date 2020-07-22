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
#' @template param_store_resample_results
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

    #' @field store_resample_results (`logical(1)`).
    store_resample_results = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measures,
      store_models = FALSE, check_values = TRUE, store_resample_results = TRUE) {
        self$task = assert_task(as_task(task, clone = TRUE))
        self$learner = assert_learner(as_learner(learner, clone = TRUE),
          task = self$task)
        self$resampling = assert_resampling(as_resampling(
          resampling, clone = TRUE))
        self$measures = assert_measures(as_measures(measures, clone = TRUE),
          task = self$task, learner = self$learner)
        self$store_models = assert_logical(store_models)
        self$store_resample_results = assert_logical(store_resample_results)
        if (!resampling$is_instantiated) {
          self$resampling$instantiate(self$task)
      }

      codomain = ParamSet$new(map(self$measures, function(s) {
          ParamDbl$new(id = s$id,
            tags = ifelse(s$minimize, "minimize", "maximize"))
        }))

      super$initialize(
        id = sprintf("%s_on_%s", self$learner$id, self$task$id), domain = self$learner$param_set,
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

      design = benchmark_grid(self$task, learners, self$resampling)
      bmr = benchmark(design, store_models = self$store_models)
      aggr = bmr$aggregate(self$measures)
      y = map_chr(self$measures, "id")

      if(self$store_resample_results) {
        rr = map(seq_len(bmr$n_resample_results), function(i) bmr$resample_result(i))
        cbind(aggr[, y, with = FALSE], resample_result = rr)
      } else {
        aggr[, y, with = FALSE]
      }
    }
  )
)
