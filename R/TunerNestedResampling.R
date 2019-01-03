#' @title TunerNestedResampling
#'
#' @description
#' Tuner child class to conduct grid search.
#'
#' @section Usage:
#' ```
#' tuner = TunerNestedResampling$new(inner_tuner, outer_resampling)
#' ```
#' See [Tuner] for a description of the interface.
#'
#' @section Arguments:
#' * `inner_tuner` ([Tuner]):
#'   Tuner that is executed on each iteration of the outer resampling.
#' * `outer_resampling` ([Resampling]):
#'   Resampling object that controls the outer tuning.
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerNestedResampling].
#' The interface is described in [Tuner].
#'
#' @name TunerNestedResampling
#' @keywords internal
#' @family Tuner
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' measures = mlr3::mlr_measures$mget("mmce")
#' param_set = paradox::ParamSet$new(
#'   params = list(paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)))
#' 
#' ff = FitnessFunction$new(task, learner, resampling, measures, param_set)
#' 
#' terminator = TerminatorEvaluations$new(5)
#' rs = TunerRandomSearch$new(ff, terminator_eval)
#' 
#' outer = mlr3::mlr_resamplings$get("cv")
#' 
#' nested = TunerNestedResampling$new(rs, outer)
#' nested$tune()
#' nested$ff$bmr$data
NULL

#' @export
#' @include Tuner.R
TunerNestedResampling = R6Class("TunerNestedResampling",
  inherit = Tuner,
  public = list(
    inner_tuner = NULL,
    outer_resampling = NULL,
    bmr_outer = NULL,

    initialize = function(inner_tuner, outer_resampling) {
      self$inner_tuner = checkmate::assert_r6(inner_tuner, "Tuner")

      self$outer_resampling = checkmate::assert_r6(outer_resampling, "Resampling")
      self$outer_resampling$instantiate(inner_tuner$ff$task)
      
      self$terminator = TerminatorIterations$new(outer_resampling$iters)
      
      self$ff = inner_tuner$ff$clone()
      self$ff$resampling$instantiate(self$ff$task)

      # Necessary to get correct reference of the terminator for the outer resampling.
      self$ff$hooks$update_start = list(self$terminator$update_start)
      self$ff$hooks$update_end = list(self$terminator$update_end)
    }
  ),

  private = list(
    tune_step = function() {
      # Deep copy is necessary to correctly copy the terminator of the inner tuningQ
      tuner_temp = self$inner_tuner$clone(deep = TRUE)

      # Necessary to get the correct reference from the tuner_temp terminator in the hooks. Otherwise,
      # the hook terminator reference points to the terminator of self$inner_tuner. This results in that the
      # tuner_temp uses his own terminator object to stop the tuning, but the hook reduces the number of remaining 
      # iterations of the self$inner_tuning terminator. Therefore, tuning is not stopped.
      tuner_temp$ff$hooks$update_start = list(tuner_temp$terminator$update_start)
      tuner_temp$ff$hooks$update_end = list(tuner_temp$terminator$update_end)

      # +1 because the terminator starts counting with 0
      iter = self$terminator$state$iters + 1L
      train_set_temp = self$outer_resampling$train_set(iter)
      tuner_temp$ff$task$filter(train_set_temp)
      tuner_temp$tune()

      # FIXME: ResamplingCustom does not work at the moment see https://github.com/mlr-org/mlr3/issues/108
      # resampling_temp = ResamplingCustom$new()
      # resampling_temp$instantiate(self$ff$task, train_sets = list(train_set_temp))
      resampling_temp = mlr3::ResamplingHoldout$new()
      self$ff$resampling = resampling_temp
      self$ff$eval(tuner_temp$tune_result()$param_vals)

      # TODO: Check how the tuner is copied and if the complete functionality is available:
      if (nrow(self$ff$bmr$data) == 1) {
        self$ff$bmr$data = cbind(self$ff$bmr$data, data.table(inner_tuner = list(tuner_temp)))
      } else {
        self$ff$bmr$data$inner_tuner[iter] = list(tuner_temp)
      }
    }
  )
)
