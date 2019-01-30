#' @title TunerRandomSearch
#'
#' @description
#' Tuner child class to conduct random search.
#'
#' @section Usage:
#' ```
#' tuner = TunerRandomSearch$new(ff, terminator, batch_size = 100L)
#' ```
#' See [Tuner] for a description of the interface.
#'
#' @section Arguments:
#' * `ff` ([FitnessFunction]):
#'   Black box function used for the tuning.
#' * `terminator` ([Terminator]):
#'   Terminator to control the termination.
#'   Will be triggered by the hooks of the [FitnessFunction].
#' * `batch_size` (`integer(1)`):
#'   Maximum number of configurations to try in a batch.
#'   Each batch is possibly executed in parallel via [mlr3::benchmark()].
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerRandomSearch].
#' The interface is described in [Tuner].
#'
#' @name TunerRandomSearch
#' @keywords internal
#' @family Tuner
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("cv")
#' resampling$param_vals$folds = 2
#' measures = mlr3::mlr_measures$mget("classif.mmce")
#' task$measures = measures
#' param_set = paradox::ParamSet$new(
#'   params = list(
#'    paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#'   )
#' )
#' ff = FitnessFunction$new(task, learner, resampling, param_set)
#'
#' terminator = TerminatorEvaluations$new(10)
#' rs = TunerRandomSearch$new(ff, terminator)
#' rs$tune()$tune_result()
NULL

#' @export
#' @include Tuner.R
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = Tuner,
  public = list(
    initialize = function(ff, terminator, batch_size = 100L) {
      batch_size = assert_count(batch_size, coerce = TRUE)
      super$initialize(id = "random_search", ff = ff, terminator = terminator, settings = list(batch_size = batch_size))
    }
  ),

  private = list(
    tune_step = function() {

      n_evals = self$terminator$settings$max_evaluations
      n_evals = if (is.null(n_evals)) n_batch else min(self$settings$batch_size, n_evals)

      design = generate_design_random(self$ff$param_set, n_evals)

      # Should be handled by paradox!
      if (nrow(design$data) > data.table::uniqueN(design$data))
        logger::log_warn("Duplicated parameter values detected.", namespace = "mlr3")

      private$eval_design_terminator(design)
    }
  )
)

