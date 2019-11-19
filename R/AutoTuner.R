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
#' * `learner` :: [mlr3::Learner]\cr
#'   Learner to tune, see [TuningInstance].
#' * `resampling` :: [mlr3::Resampling]\cr
#'   Resampling strategy during tuning, see [TuningInstance].
#'   This [mlr3::Resampling] is meant to be the **inner** resampling, operating on the training set
#'   of an arbitrary outer resampling.
#'   For this reason it is not feasible to pass an instantiated [mlr3::Resampling] here.
#' * `measures` :: list of [mlr3::Measure]\cr
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
#' * `instance_args` :: `list`
#'      All arguments from construction to create the [TuningInstance].
#' * `tuner` :: [Tuner]; from construction.
#' * `store_tuning_instance` :: `logical(1)`\cr
#'   If `TRUE`, stores the internally created [TuningInstance] with all intermediate results in slot `$tuning_instance`.
#'   By default, this is `TRUE`.
#' * `learner` :: [mlr3::Learner]
#'      Trained learner
#' * `tuning_instance` :: [TuningInstance]\cr
#'   Internally created tuning instance with all intermediate results.
#' * `tuning_result` :: named `list`\cr
#'    Short-cut to `result` from [TuningInstance].
#'
#' @section Methods:
#' All methods from [Learner], and additionally:
#'
#' * `archive(unnest = "params")`\cr
#'   `character(1)` -> [data.table::data.table()]\cr
#'    Short-cut to method from [TuningInstance].
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
    instance_args = NULL,
    tuner = NULL,
    store_tuning_instance = TRUE,

    initialize = function(learner, resampling, measures, tune_ps, terminator, tuner, bm_args = list()) {
      ia = list()
      ia$learner = assert_learner(learner)$clone()
      ia$resampling = assert_resampling(resampling, instantiated = FALSE)$clone()
      ia$measures = assert_measures(as_measures(measures), learner = learner)
      ia$param_set = assert_param_set(tune_ps)$clone()
      ia$learner$param_set$set_id = "" # FIXME: i have no idea why we do this here?
      ia$terminator = assert_terminator(terminator)$clone()
      ia$bm_args = assert_list(bm_args, names = "unique")
      self$instance_args = ia
      self$tuner = assert_tuner(tuner)$clone()

      super$initialize(
        id = paste0(learner$id, ".tuned"),
        task_type = learner$task_type,
        packages = learner$packages,
        feature_types = learner$feature_types,
        predict_types = learner$predict_types,
        param_set = learner$param_set,
        properties = learner$properties
      )

      self$predict_type = learner$predict_type
    },

    train_internal = function(task) {
      # construct instance from args; then tune
      ia = self$instance_args
      ia$task = task
      instance = do.call(TuningInstance$new, ia)
      self$tuner$tune(instance)

      # get learner, set params to optimal, then train
      learner = ia$learner
      learner$param_set$values = instance$result$params
      learner$train(task)

      # the return model is a list of "learner" and "tuning_instance"
      result_model = list()
      result_model$learner = learner
      if (isTRUE(self$store_tuning_instance)) {
        result_model$tuning_instance = instance
      }
      return(result_model)
    },

    predict_internal = function(task) {
      self$model$learner$predict(task)
    },

    archive = function(unnest = "no") self$tuning_instance$archive(unnest)
  ),

  active = list(

    learner = function()  {
      # if there is no trained learner, we return the one in instance args
      if (is.null(self$model))
        self$instance_args$learner
      else
        self$model$learner
    },
    tuning_instance = function() self$model$tuning_instance,
    tuning_result = function() self$tuning_instance$result,

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
    }
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
