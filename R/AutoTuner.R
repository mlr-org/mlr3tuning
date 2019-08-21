#' @title AutoTuner
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Learner].
#'
#' @description
#' The `AutoTuner` is a [mlr3::Learner] which tunes a subordinate learner via resampling.
#' The best found configuration is then used to train a model on the complete training data.
#' Note that this class allows to perform nested resampling by passing an [AutoTuner] object to [mlr3::resample()]
#' or [mlr3::benchmark()].
#'
#' @section Construction:
#' ```
#' at = AutoTuner$new(learner, resampling, measures, param_set, terminator,
#'   tuner, bm_args = list(), id = "autotuner")
#' ```
#' * `learner` :: [mlr3::Learner] | [mlr3::mlr_sugar]\cr
#'   Learner to tune.
#' * `resampling` :: [mlr3::Resampling] | [mlr3::mlr_sugar]\cr
#'   Resampling strategy used to assess the performance of the learner on the (subset of) the
#' * `measures` :: list of [mlr3::Measure] | [mlr3::mlr_sugar]\cr
#'   Performance measures. The first one is optimized.
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Hyperparameter search space.
#' * `terminator` :: [Terminator]\cr
#'   When to stop tuning.
#' * `tuner` :: [Tuner]\cr
#'   Tuning algorithm to run.
#' * `bm_args` :: `list()`\cr
#'   Further arguments for [mlr::benchmark()], see [TuningInstance].
#' * `id` :: `character(1)`\cr
#'   Name of the learner.
#'
#' @section Fields:
#' All fields from [Learner], and additionally:
#'
#' * `learner` :: [mlr3::Learner]\cr
#'   Subordinate learner. After `train()` of the `AutoTuner` has been executed,
#'   this learner stores the final model and is parametrized with the best found solution.
#' * `resampling` :: [mlr3::Resampling].
#' * `measures` :: list of [mlr3::Measure].
#' * `tune_ps` :: [paradox::ParamSet]\cr
#'   Hyperparameter search space.
#' * `terminator` :: [Terminator].
#' * `tuner` :: [Tuner].
#' * `store_bmr` :: `logical(1)`\cr
#'   If `TRUE`, stores the benchmark result as slot `$bmr`.
#' * `tune_path` :: [data.table::data.table()]\cr
#'   Only stored if `store_bmr` has been set to `TRUE`.
#'   This is the archive of the stored [mlr3::BenchmarkResult] with hyperparameters as separate columns.
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
#' terminator = TerminatorEvals$new(5)
#' tuner = TunerGridSearch$new()
#' at = AutoTuner$new(learner, resampling, measures, param_set, terminator, tuner)
#' at$store_bmr = TRUE
#'
#' at$train(task)
#' at$model
#' at$learner
AutoTuner = R6Class("AutoTuner", inherit = Learner,
  public = list(
    # FIXME: doesnt thus store the instance? and hence the complete bmr and all other slots?
    learner = NULL,
    terminator = NULL,
    resampling = NULL,
    measures = NULL,
    tuner = NULL,
    tune_ps = NULL,
    store_bmr = FALSE,
    bm_args = NULL,
    # FIXME: state = bmr + learner
    # FIXME: look at pipeopelearner for paramset
    # FIXME: autotuner paramset darf eigentlich nicht die params mehr enthalten über die getuned wird

    initialize = function(learner, resampling, measures, tune_ps, terminator, tuner, bm_args = list(), id = "autotuner") {
      self$id = assert_string(id) # needs to be set first for param_set active binding
      self$tuner = assert_r6(tuner, "Tuner")
      self$learner = learner = assert_learner(learner = as_learner(learner, clone = TRUE))
      self$learner$param_set$set_id = ""
      self$terminator = assert_r6(terminator, "Terminator")
      self$resampling = assert_resampling(as_resampling(resampling, clone = TRUE))
      self$measures = assert_measures(as_measures(measures, clone = TRUE), learner = learner)
      self$tune_ps = assert_class(tune_ps, "ParamSet")
      self$bm_args = assert_list(bm_args, names = "unique")

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
      tres = self$tuner$tune(instance)
      learner$param_set$values = tres$values

      # train internal learner
      model = list(learner = learner$train(task))

      if (isTRUE(self$store_bmr)) {
        model$bmr = instance$bmr
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
