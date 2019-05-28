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
#' task$measures = measures
#' param_set = paradox::ParamSet$new(
#'   params = list(paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1)))
#'
#' terminator = TerminatorEvaluations$new(5)
#'
#' at = AutoTuner$new(learner, resampling, param_set, terminator, tuner = TunerGridSearch,
#'   tuner_settings = list(resolution = 10L))
#'
#' at$train(task)
#' at$learner
NULL

#' @export
AutoTuner = R6Class("AutoTuner", inherit = mlr3::Learner,
  public = list(
    learner = NULL,

    initialize = function(learner, resampling, param_set, terminator, tuner, tuner_settings, ctrl = tune_control(), id = "autotuner") {

      self$learner = mlr3::assert_learner(learner = learner)

      private$.terminator = checkmate::assert_r6(terminator, "Terminator")
      private$.tuner_settings = checkmate::assert_list(tuner_settings)
      private$.ff_args$resampling = mlr3::assert_resampling(resampling)
      private$.ff_args$param_set = checkmate::assert_class(param_set, "ParamSet")
      private$.ff_args$ctrl = checkmate::assert_list(ctrl, names = "unique")

      # TODO: Check for factory
      if (!inherits(tuner, "R6ClassGenerator") && grepl(pattern = "Tuner", x = tuner$classname)) {
        mlr3misc::stopf("Tuner must be a R6 class generator that creates tuner (e.g. TunerGridSearch).")
      }
      private$.tuner = tuner

      super$initialize(
        id = id,
        task_type = self$learner$task_type,
        packages = self$learner$packages,
        feature_types = self$learner$feature_types,
        predict_types = self$learner$predict_types,
        param_set = self$learner$param_set,
        properties = self$learner$properties
      )
    },

    train = function(task) {
      if (private$.is_trained) {
        logger::log_warn("Learner is already trained.", namespace = "mlr3")
      } else {
        self$learner = mlr3::assert_learner(learner = self$learner, task = task)

        private$.tuner_settings$terminator = private$.terminator$clone()
        private$.tuner_settings$ff = FitnessFunction$new(
          task = mlr3::assert_task(task)$clone(deep = TRUE),
          learner = self$learner$clone(deep = TRUE),
          resampling = private$.ff_args$resampling$clone(deep = TRUE),
          param_set = private$.ff_args$param_set$clone(deep = TRUE),
          ctrl = private$.ff_args$ctrl
        )

        private$.tuner = do.call(private$.tuner$new, private$.tuner_settings)
        private$.tuner$tune()

        self$learner$param_set$values = private$.tuner$tune_result()$values
        self$learner$train(task)

        private$.is_trained = TRUE
        self$model = self$learner$model
      }
      self
    },

    predict = function(task) {
      self$learner$predict(task)
    }
  ),

  private = list(
    .ff_args = NULL,
    .terminator = NULL,
    .tuner = NULL,
    .tuner_settings = NULL,
    .is_trained = FALSE,

    deep_clone = function(name, value) {
      if (R6::is.R6(value)) {
        return(value$clone(deep = TRUE))
      } else {
        return(value)
      }
    }
  ),

  active = list(
    tuner = function(rhs) {
      if (!missing(rhs)) stop("tuner is read only")
      if (private$.is_trained) return(private$.tuner)

      return(NULL)
    }
  )
)
