#' @title Context
#'
#' @description
#' This is the abstract base class for context objects.
#' Context objects allow [Callback] objects to access and modify data.
#' Access to data can be restricted with active bindings (see example).
#'
#' @export
#' @examples
#' library(data.table)
#' library(R6)
#'
#' # data table with column x an y
#' data = data.table(x = runif(10), y = sample(c("A", "B"), 10, replace = TRUE))
#'
#' # context only allows to access column y
#' ContextExample = R6Class("ContextExample",
#'   inherit = mlr3misc::Context,
#'   public = list(
#'     data = NULL,
#'
#'     initialize = function(data) {
#'         self$data = data
#'     }
#'   ),
#'
#'   active = list(
#'     y = function(rhs) {
#'       if (missing(rhs)) return(self$data$y)
#'       self$data$y = rhs
#'     }
#'   )
#' )
#'
#' context = ContextExample$new(data)
#'
#' # retrieve content of column y
#' context$y
#'
#' # change content of column y to "C"
#' context$y = "C"
ContextEval = R6Class("ContextEval",
  inherit = mlr3misc::Context,
  public = list(

    #' @field objective_tuning [ObjectiveTuning].
    objective_tuning = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the new callback.
    #' @param objective_tuning [ObjectiveTuning].
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

    #' @field benchmark_result ([mlr3::BenchmarkResult])\cr
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
