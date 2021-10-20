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

    #' @field hotstart_stack ([mlr3::HotstartStack]).
    hotstart_stack = NULL,

    #' @field allow_hotstart (`logical(1)`).
    allow_hotstart = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(task, learner, resampling, measures, check_values = TRUE, store_benchmark_result = TRUE,
      store_models = FALSE, allow_hotstart = FALSE) {

      self$task = assert_task(as_task(task, clone = TRUE))
      self$learner = assert_learner(as_learner(learner, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), task = self$task, learner = self$learner)
      self$store_benchmark_result = assert_logical(store_benchmark_result)
      self$allow_hotstart = assert_logical(allow_hotstart) && any(c("hotstart_forward", "hotstart_backward") %in% learner$properties)
      self$store_models = assert_logical(store_models) || self$allow_hotstart

      codomain = ParamSet$new(map(self$measures, function(s) {
        ParamDbl$new(id = s$id, tags = ifelse(s$minimize, "minimize", "maximize"))
      }))

      super$initialize(id = sprintf("%s_on_%s", self$learner$id, self$task$id), properties = "noisy",
        domain = self$learner$param_set, codomain = codomain, constants = ps(resampling = p_uty()),
        check_values = check_values)

      # set resamplings in constants
      resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      if (!resampling$is_instantiated) resampling$instantiate(task)
      self$resampling = resampling
      self$constants$values$resampling = list(resampling)
    }
  ),

  private = list(
    .eval_many = function(xss, resampling) {
      learners = map(xss, function(x) {
        learner = self$learner$clone(deep = TRUE)
        learner$param_set$values = insert_named(learner$param_set$values, x)
        if (self$allow_hotstart) learner$hotstart_stack = self$hotstart_stack
        learner
      })

      design = data.table(task = list(self$task), learner = learners, resampling = resampling)
      bmr = benchmark(design, store_models = self$store_models, allow_hotstart = self$allow_hotstart)
      aggr = bmr$aggregate(self$measures)
      y = map_chr(self$measures, "id")

      # add runtime
      time = map_dbl(bmr$resample_results$resample_result, function(rr) {
        sum(map_dbl(rr$learners, function(l) sum(l$timings)))
      })
      aggr[, "runtime_learners" := time]

      if (self$store_benchmark_result) {
        if (is.null(self$archive$benchmark_result)) {
          self$archive$benchmark_result = bmr
          if (self$allow_hotstart) self$hotstart_stack = HotstartStack$new(extract_bmr_learners(bmr))
        } else {
          self$archive$benchmark_result$combine(bmr)
          if (self$allow_hotstart) self$hotstart_stack$add(extract_bmr_learners(bmr))
        }
        cbind(aggr[, c(y, "runtime_learners"), with = FALSE], uhash = bmr$uhashes)
      } else {
        aggr[, c(y, "runtime_learners"), with = FALSE]
      }
    }
  )
)

extract_bmr_learners = function(bmr) {
  unlist(map(seq_len(bmr$n_resample_results), function(n) {
    bmr$resample_result(n)$learners
  }))
}
