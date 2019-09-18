#' @title AutoTuner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Learner].
#'
#' @description
#' The `AutoTuner` is a [mlr3::Learner] which auto-tunes by first tuning the hyperparameters of its encapsulated learner
#' on the training data, then setting the optimal configuration in the learner, then finally
#' fitting the model on the complete training data.
#' Note that this class allows to perform nested resampling by passing an [AutoTuner] object to [mlr3::resample()]
#' or [mlr3::benchmark()].
#'
#' @section Construction:
#' ```
#' at = AutoTuner$new(learner, resampling, measures, tune_ps, terminator, tuner, bm_args = list())
#' ```
#' * `learner` :: [mlr3::Learner] | [mlr3::mlr_sugar]\cr
#'   Learner to tune, see [TuningInstance].
#' * `resampling` :: [mlr3::Resampling] | [mlr3::mlr_sugar]\cr
#'   Resampling strategy during tuning, see [TuningInstance].
#' * `measures` :: list of [mlr3::Measure] | [mlr3::mlr_sugar]\cr
#'   Performance measures. The first one is optimized, see [TuningInstance].
#' * `tune_ps` :: [paradox::ParamSet]\cr
#'   Hyperparameter search space, see [TuningInstance].
#' * `terminator` :: [Terminator]\cr
#'   When to stop tuning, see [TuningInstance].
#' * `tuner` :: [Tuner]\cr
#'   Tuning algorithm to run.
#' * `bm_args` :: `list()`\cr
#'   Further arguments for [mlr3::benchmark()], see [TuningInstance].
#'
#' @section Fields:
#' All fields from [Learner], and additionally:
#'
#' * `learner` :: [mlr3::Learner]; from construction.
#' * `resampling` :: [mlr3::Resampling]; from construction.
#' * `measures` :: list of [mlr3::Measure]; from construction.
#' * `tune_ps` :: [paradox::ParamSet]; from construction.
#' * `terminator` :: [Terminator]; from construction.
#' * `tuner` :: [Tuner]; from construction.
#' * `store_tuning_instance` :: `logical(1)`\cr
#'   If `TRUE`, stores the internally created [TuningInstance] with all intermediate results in slot `$tuning_instance`.
#'   By default, this is `TRUE`.
#' * `tuning_instance` :: [TuningInstance]\cr
#'   Internally created tuning instance with all intermediate results.
#'
#' @section Methods:
#' See [mlr3::Learner].
#'
#' @family Learner
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("holdout")
#' measures = msr("classif.ce")
#' param_set = ParamSet$new(
#'   params = list(ParamDbl$new("cp", lower = 0.001, upper = 0.1)))
#'
#' terminator = term("evals", n_evals = 5)
#' tuner = tnr("grid_search")
#' at = AutoTuner$new(learner, resampling, measures, param_set, terminator, tuner)
#' at$store_tuning_instance = TRUE
#'
#' at$train(task)
#' at$model
#' at$learner
AutoTuner = R6Class("AutoTuner", inherit = Learner,
  public = list(
    learner = NULL,
    terminator = NULL,
    resampling = NULL,
    measures = NULL,
    tuner = NULL,
    tune_ps = NULL,
    store_tuning_instance = TRUE,
    bm_args = NULL,
    # tuning_instance = NULL,

    initialize = function(learner, resampling, measures, tune_ps, terminator, tuner, bm_args = list()) {
      self$learner = assert_learner(learner)
      self$resampling = assert_resampling(resampling)
      self$measures = assert_measures(as_measures(measures), learner = learner)
      self$tune_ps = assert_param_set(tune_ps)
      self$learner$param_set$set_id = "" # FIXME: i have no idea why we do this here?
      self$terminator = assert_terminator(terminator)
      self$tuner = assert_tuner(tuner)
      self$bm_args = assert_list(bm_args, names = "unique")

      super$initialize(
        id = paste0(self$learner$id, ".tuned"),
        task_type = self$learner$task_type,
        packages = self$learner$packages,
        feature_types = self$learner$feature_types,
        predict_types = self$learner$predict_types,
        param_set = self$learner$param_set,
        properties = self$learner$properties
      )
    },

    train_internal = function(task) {

      terminator = self$terminator$clone()
      learner = self$learner$clone(deep = TRUE)
      instance = TuningInstance$new(
        task = assert_task(as_task(task, clone = TRUE)),
        learner = learner,
        resampling = self$resampling,
        measures = self$measures,
        param_set = self$tune_ps,
        terminator = self$terminator,
        bm_args = self$bm_args
      )

      # update param vals
      self$tuner$tune(instance)
      learner$param_set$values = instance$result(complete = TRUE)$config_trafo

      # train internal learner
      model = list(learner = learner$train(task))

      if (isTRUE(self$store_tuning_instance)) {
        model$tuning_instance = instance
      }

      model
    },

    predict_internal = function(task) {
      self$model$learner$predict(task)
    }
  ),

  active = list(
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        private$.param_set = ParamSetCollection$new(list(
          # --> this is how we would insert the self$tuner_paramset:
          # self$tuner$param_set,
          self$learner$param_set
        ))
        private$.param_set$set_id = private$.ps_id
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },

    tuning_instance = function() self$model$tuning_instance
  ),
  private = list(
    deep_clone = function(name, value) {
      if (!is.null(private$.param_set)) {
        private$.ps_id = private$.param_set$set_id
        private$.param_set = NULL # required to keep clone identical to original, otherwise tests get really ugly
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    .ps_id = ""
  )
)
