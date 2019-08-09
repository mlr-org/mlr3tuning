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
#'   tuner, tuner_settings, store_models = FALSE, id = "autotuner")
#' ```
#' * `learner` :: [mlr3::Learner]\cr
#'   Subordinate learner to tune.
#' * `resampling` :: [mlr3::Resampling]\cr
#'   Resampling strategy used to assess the performance of the learner on the (subset of) the
#'   [Task] passed to `$train()`.
#' * `measures` :: list of [mlr3::Measure]\cr
#'   Performance measures. The first one is subject to tuning.
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Tuning space.
#' * `terminator` :: [Terminator]\cr
#'   Controls the terminator of the `tuner`.
#' * `tuner` :: [Tuner]\cr
#'   Uninitialized tuner factory, e.g. TunerGridSearch.
#' * `tuner_settings` :: named `list()`\cr
#'   List with tuner settings (e.g. see [TunerGridSearch])
#' * `store_models` :: `logical(1)`\cr
#'   Keep the fitted learner models? Passed down to [mlr3::benchmark()].
#' * `id` :: `character(1)`\cr
#'   Name of the learner.
#'
#' @section Fields:
#' All fields from [Learner], and additionally:
#'
#' * `learner` :: [mlr3::Learner]\cr
#'   Subordinate learner. After `train()` of the `AutoTuner` has been executed,
#'   this learner stores the final model on is parametrized with the best found solution.
#'
#' * `store_bmr` :: `logical(1)`\cr
#'   If `TRUE`, store the benchmark result as slot `$bmr`.
#'
#' * `bmr` :: [mlr3::BenchmarkResult]\cr
#'   Only stored if `store_bmr` has been set to `TRUE`.
#'   This object acts as an optimization path.
#'
#' * `tuner` :: [Tuner]\cr
#'   Access to the stored [Tuner].
#'
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
#' task = mlr_tasks$get("iris")
#' learner = mlr_learners$get("classif.rpart")
#' resampling = mlr_resamplings$get("holdout")
#' measures = mlr_measures$mget("classif.ce")
#' param_set = ParamSet$new(
#'   params = list(ParamDbl$new("cp", lower = 0.001, upper = 0.1)))
#'
#' terminator = TerminatorEvaluations$new(5)
#'
#' at = AutoTuner$new(learner, resampling, measures, param_set, terminator, tuner = TunerGridSearch,
#'   tuner_settings = list(resolution = 10L))
#' at$store_bmr = TRUE
#'
#' at$train(task)
#' at$model
#' at$learner
AutoTuner = R6Class("AutoTuner", inherit = Learner,
  public = list(
    tuner_generator = NULL,
    #FIXME: doesnt thus store the PE? and hence the complete bmr and all other slots?
    tuner = NULL,
    learner = NULL,
    terminator = NULL,
    tuner_settings = NULL,
    resampling = NULL,
    measures = NULL,
    tune_ps = NULL,
    store_bmr = FALSE,
    bmr = NULL,
    store_models = FALSE,
    # FIXME: state = bmr + learner
    # FIXME: look at pipeopelearner for paramset
    # FIXME: autotuner paramset darf eigentlich nicht die params mehr enthalten über die getuned wird

    initialize = function(learner, resampling, measures, tune_ps, terminator, tuner, tuner_settings = list(), store_models = FALSE, id = "autotuner") {
      if (!inherits(tuner, "R6ClassGenerator") && grepl(pattern = "Tuner", x = tuner$classname)) {
        stopf("Tuner must be a R6 class generator that creates tuner (e.g. TunerGridSearch).")
      }

      self$tuner_generator = tuner
      self$learner = learner = assert_learner(learner = learner)
      self$terminator = assert_r6(terminator, "Terminator")
      self$tuner_settings = assert_list(tuner_settings, names = "unique")
      self$resampling = assert_resampling(resampling)
      self$measures = assert_measures(measures)
      self$tune_ps = assert_class(tune_ps, "ParamSet")
      self$store_models = assert_flag(store_models)

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
      pe = PerfEval$new(
        task = assert_task(task)$clone(deep = TRUE),
        learner = self$learner$clone(deep = TRUE),
        resampling = self$resampling$clone(deep = TRUE),
        measures  = self$measures,
        param_set = self$tune_ps$clone(deep = TRUE),
        store_models = self$store_models
      )

      tuner = do.call(self$tuner_generator$new, insert_named(self$tuner_settings, list(pe = pe, terminator = terminator)))
      self$tuner = tuner$tune()

      # update param vals
      self$learner$param_set$values = tuner$tune_result()$values

      # train internal learner
      self$learner$train(task)

      if (isTRUE(self$store_bmr)) {
        self$bmr = pe$bmr
      }

      return(self$learner$model)
    },

    predict_internal = function(task) {
      self$learner$predict_internal(task)
    },

    new_prediction = function(row_ids, truth, ...) {
      self$learner$new_prediction(row_ids, truth, ...)
    },

    archive = function(unnest = TRUE) self$tuner$archive(unnest)
  ),

  active = list(
    # learner = function() {
    #   self$learner
    # },

    # model = function() {
    #   self$learner$model
    # },

    # tuner = function() {
    #   self$tuner
    # },

    # bmr = function() {
      # self$bmr
    # },

  )
)
