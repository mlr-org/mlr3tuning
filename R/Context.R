ContextEval = R6Class("ContextEval",
  public = list(
    objective_tuning = NULL,

    initialize = function(objective_tuning) {
      self$objective_tuning = assert_r6(objective_tuning, "ObjectiveTuning")
    }
  ),

  active = list(
    #' @field design ([data.table::data.table])\cr
    #'   The benchmark design of the latest batch.
    design = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.design)
      } else {
        get_private(self$objective_tuning)$.design = rhs
      }
    },

    #' @field bmr ([mlr3::BenchmarkResult])\cr
    #'   The benchmark result of the latest batch.
    benchmark_result = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.benchmark_result)
      } else {
        get_private(self$objective_tuning)$.benchmark_result = rhs
      }
    },

    #' @field aggregated_performance ([data.table::data.table])\cr
    #'   Aggregated performance scores of the latest batch.
    aggregated_performance = function(rhs) {
      if (missing(rhs)) {
        return(get_private(self$objective_tuning)$.aggregated_performance)
      } else {
        get_private(self$objective_tuning)$.aggregated_performance = rhs
      }
    }
  )
)
