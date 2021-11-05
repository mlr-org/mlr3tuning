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
#' @template param_allow_hotstart
#' @template param_keep_hotstart_stack
#' @template param_learner_limit
#'
#' @export
ObjectiveTuningAsync = R6Class("ObjectiveTuningAsync",
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

    #' @field hotstart_stack ([mlr3::HotstartStack]).
    hotstart_stack = NULL,

    #' @field allow_hotstart (logical(1))\cr
    allow_hotstart = NULL,

    #' @field keep_hotstart_stack (`logical(1)`).
    keep_hotstart_stack = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measures, check_values = TRUE, store_benchmark_result = TRUE,
      store_models = FALSE, allow_hotstart = FALSE, keep_hotstart_stack = FALSE, learner_limit = NULL) {

      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)
      self$store_benchmark_result = assert_logical(store_benchmark_result)
      self$allow_hotstart = assert_logical(allow_hotstart) && any(c("hotstart_forward", "hotstart_backward") %in% learner$properties)
      if (self$allow_hotstart) self$hotstart_stack = HotstartStackDB$new(learner_limit = learner_limit)
      self$keep_hotstart_stack = assert_flag(keep_hotstart_stack)
      self$store_models = assert_logical(store_models)

      codomain = ParamSet$new(map(self$measures, function(s) {
        ParamDbl$new(id = s$id, tags = ifelse(s$minimize, "minimize", "maximize"))
      }))

      super$initialize(id = sprintf("%s_on_%s", self$learner$id, self$task$id), domain = self$learner$param_set,
        codomain = codomain, constants = ps(resampling = p_uty()), check_values = check_values)

      # set resamplings in constants
      resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      if (!resampling$is_instantiated) resampling$instantiate(task)
      self$resampling = resampling
      self$constants$values$resampling = list(resampling)
    }
  ),

  private = list(
    .eval = function(xs, resampling) {
      learner = self$learner$clone(deep = TRUE)
      learner$param_set$values = insert_named(learner$param_set$values, xs)
      if (self$allow_hotstart) learner$hotstart_stack = self$hotstart_stack

      rr = resample(self$task, learner, resampling[[1]], store_models = self$store_models || self$allow_hotstart, allow_hotstart = self$allow_hotstart)
      aggr = rr$aggregate(self$measures)
      time = sum(map_dbl(rr$learners, function(l) sum(l$timings)))

      ys = c(as.list(aggr), runtime_learners = time)
      if (self$store_benchmark_result || self$allow_hotstart) ys = c(ys, resample_result = list(list(rr)))
      ys
    }
  )
)
