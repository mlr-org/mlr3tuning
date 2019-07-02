#' @title TunerRandomSearch
#'
#' @include Tuner.R
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Tuner].
#'
#' @description
#' Tuner child class to conduct a random search.
#'
#' @section Construction:
#' ```
#' tuner = TunerRandomSearch$new(pe, terminator, batch_size = 100L)
#' ```
#' For arguments, see [Tuner], and additionally:
#'
#' * `batch_size` :: `integer(1)`\cr
#'   Maximum number of configurations to try in a batch.
#'   Each batch is possibly executed in parallel via [mlr3::benchmark()].
#'
#' @section Fields:
#' See [Tuner].
#'
#' @section Methods:
#' See [Tuner].
#'
#' @family Tuner
#' @export
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv")
#' resampling$param_set$values$folds = 2
#' measures = mlr3::mlr_measures$mget("classif.ce")
#' param_set = paradox::ParamSet$new(
#'   params = list(
#'     paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#'   )
#' )
#' pe = PerformanceEvaluator$new(task, learner, resampling, measures, param_set)
#'
#' terminator = TerminatorEvaluations$new(10)
#' rs = TunerRandomSearch$new(pe, terminator)
#' rs$tune()$tune_result()
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = Tuner,
  public = list(
    initialize = function(pe, terminator, batch_size = 100L) {
      batch_size = assert_count(batch_size, coerce = TRUE)
      super$initialize(id = "random_search", pe = pe, terminator = terminator, settings = list(batch_size = batch_size))
    }
  ),

  private = list(
    tune_step = function() {
      n_evals = self$terminator$settings$max_evaluations
      n_evals = if (is.null(n_evals)) self$settings$batch_size else min(self$settings$batch_size, n_evals)

      design = generate_design_random(self$pe$param_set, n_evals)

      # Should be handled by paradox!
      if (nrow(design$data) > data.table::uniqueN(design$data)) {
        lg$warn("Duplicated parameter values detected")
      }

      private$eval_design_terminator(design)
    }
  )
)
