#' @title AutoTuner
#'
#' @description
#' The [AutoTuner] conducts tuning and sets the learners parameter configuration to the best parameters obtained by the tuning.
#'
#' Additionally, this class can be used to do nested resampling by passing an [AutoTuner] object to resample.
#'
#' @section Usage:
#' ```
#' # Construction
#' at = AutoTuner$new(learner, resampling, param_set, terminator,
#'   tuner, tuner_settings, ctrl = tune_control(), id = "autotuner")
#'
#' # public fields
#' at$learner
#' ```
#' See [Learner] for a description of the interface.
#'
#' @section Arguments:
#' * `learner` ([Learner]): \cr
#'   Internal learner that is tuned and finally returned as trained learner with optimal parameter configuration.
#' * `resampling` ([Resampling]): \cr
#'   Resampling strategy for the tuning.
#' * `param_set` ([paradox::ParamSet]) \cr
#'   Parameter set for the tuning.
#' * `terminator` ([Terminator]) \cr
#'   Terminator used to stop the tuning.
#' * `tuner` (Tuner Class Generator) \cr
#'   Uninitialized tuner factory, e.g. TunerGridSearch.
#' * `tuner_settings` (named list) \cr
#'   List with tuner settings (e.g. see `?TunerGridSearch`)
#'
#' @section Details:
#' * With `at$learner` the raw learner with the best parameter configuration can be accessed.
#'
#' The interface is described in [Learner].
#'
#' @name AutoTuner
#' @keywords internal
#' @family Learner
#' @examples
#' task = mlr3::mlr_tasks$get("iris")
#' learner = mlr3::mlr_learners$get("classif.rpart")
#' resampling = mlr3::mlr_resamplings$get("holdout")
#' measures = mlr3::mlr_measures$mget("classif.ce")
#' param_set = paradox::ParamSet$new(
#'   params = list(paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)))
#'
#' terminator = TerminatorEvaluations$new(5)
#'
#' at = AutoTuner$new(learner, resampling, measures, param_set, terminator, tuner = TunerGridSearch,
#'   tuner_settings = list(resolution = 10L))
#'
#' at$train(task)
#' at$learner
NULL

#' @export
AutoTuner = R6Class("AutoTuner", inherit = mlr3::Learner,
  public = list(
    initialize = function(learner, resampling, measures, param_set, terminator, tuner, tuner_settings, ctrl = tune_control(), id = "autotuner") {
      # TODO: Check for factory
      if (!inherits(tuner, "R6ClassGenerator") && grepl(pattern = "Tuner", x = tuner$classname)) {
        stopf("Tuner must be a R6 class generator that creates tuner (e.g. TunerGridSearch).")
      }

      self$data$tuner_generator = tuner
      self$data$learner = learner = mlr3::assert_learner(learner = learner)
      self$data$terminator = checkmate::assert_r6(terminator, "Terminator")
      self$data$tuner_settings = checkmate::assert_list(tuner_settings)
      self$data$resampling = mlr3::assert_resampling(resampling)
      self$data$measures = mlr3::assert_measures(measures)
      self$data$param_set = checkmate::assert_class(param_set, "ParamSet")

      super$initialize(
        id = id,
        task_type = learner$task_type,
        packages = learner$packages,
        feature_types = learner$feature_types,
        predict_types = learner$predict_types,
        param_set = learner$param_set,
        properties = learner$properties
      )
    },

    train_internal = function(task) {
      terminator = self$data$terminator$clone()
      pe = PerformanceEvaluator$new(
        task = mlr3::assert_task(task)$clone(deep = TRUE),
        learner = self$data$learner$clone(deep = TRUE),
        resampling = self$data$resampling$clone(deep = TRUE),
        measures  = self$data$measures,
        param_set = self$data$param_set$clone(deep = TRUE)
      )

      tuner = do.call(self$data$tuner_generator$new, insert_named(self$data$tuner_settings, list(pe = pe, terminator = terminator)))
      self$data$tuner = tuner$tune()

      # update param vals
      self$param_set$values = self$data$learner$param_set$values = tuner$tune_result()$values

      # train internal learner
      self$data$learner$train(task)

      return(self$data$learner$model)
    },

    predict_internal = function(task) {
      self$data$learner$predict_internal(task)
    },

    new_prediction = function(row_ids, truth, ...) {
      self$data$learner$new_prediction(row_ids, truth, ...)
    }
  ),

  active = list(
    learner = function() {
      self$data$learner
    },

    model = function() {
      self$data$learner$model
    },

    tuner = function(rhs) {
      if (!missing(rhs)) stop("tuner is read only")
      self$data$tuner
    }
  )
)
