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
#'   tuner, tuner_settings, ctrl = list(), id = "autotuner")
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
#' * `ctrl` :: named `list()`\cr
#'   See [mlr3::mlr_control()].
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
#' @section Methods:
#' See [mlr3::Learner].
#'
#' @family Learner
#' @export
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
#' at$store_bmr = TRUE
#'
#' at$train(task)
#' at$model
#' at$learner
#'
#' # retrieve the best ResampleResult
#' rr = at$bmr$best(measures)
#' rr$aggregate(measures)
AutoTuner = R6Class("AutoTuner", inherit = mlr3::Learner,
  public = list(
    store_bmr = FALSE,

    initialize = function(learner, resampling, measures, param_set, terminator, tuner, tuner_settings = list(), ctrl = list(), id = "autotuner") {
      if (!inherits(tuner, "R6ClassGenerator") && grepl(pattern = "Tuner", x = tuner$classname)) {
        stopf("Tuner must be a R6 class generator that creates tuner (e.g. TunerGridSearch).")
      }

      self$data$tuner_generator = tuner
      self$data$learner = learner = mlr3::assert_learner(learner = learner)
      self$data$terminator = assert_r6(terminator, "Terminator")
      self$data$tuner_settings = assert_list(tuner_settings, names = "unique")
      self$data$resampling = mlr3::assert_resampling(resampling)
      self$data$measures = mlr3::assert_measures(measures)
      self$data$param_set = assert_class(param_set, "ParamSet")

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

      if (isTRUE(self$store_bmr)) {
        self$data$bmr = pe$bmr
      }

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

    tuner = function() {
      self$data$tuner
    },

    bmr = function() {
      self$data$bmr
    }
  )
)
