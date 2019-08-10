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
#' terminator = TerminatorEvals$new(5)
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
    learner = NULL,
    terminator = NULL,
    resampling = NULL,
    measures = NULL,
    tuner_settings = NULL,
    tune_ps = NULL,
    store_bmr = FALSE,
    store_models = FALSE,
    # FIXME: state = bmr + learner
    # FIXME: look at pipeopelearner for paramset
    # FIXME: autotuner paramset darf eigentlich nicht die params mehr enthalten über die getuned wird

    initialize = function(learner, resampling, measures, tune_ps, terminator, tuner, tuner_settings = list(), store_models = FALSE, id = "autotuner") {
      if (!inherits(tuner, "R6ClassGenerator") && grepl(pattern = "Tuner", x = tuner$classname)) {
        stopf("Tuner must be a R6 class generator that creates tuner (e.g. TunerGridSearch).")
      }
      self$id = id  # needs to be set first for param_set active binding

      self$tuner_generator = tuner
      self$learner = learner = assert_learner(learner = learner, clone = TRUE)
      self$learner$param_set$set_id = ""
      self$terminator = assert_r6(terminator, "Terminator")
      self$tuner_settings = assert_list(tuner_settings, names = "unique")
      self$resampling = assert_resampling(resampling, clone = TRUE)
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
      learner = self$learner$clone(deep = TRUE)
      pe = PerfEval$new(
        task = assert_task(task, clone = TRUE),
        learner = learner,
        resampling = self$resampling,
        measures  = self$measures,
        param_set = self$tune_ps,
        terminator = self$terminator,
        store_models = self$store_models
      )

      tuner = do.call(self$tuner_generator$new, self$tuner_settings)

      # update param vals
      tres = tuner$tune(pe)
      learner$param_set$values = tres$values

      # train internal learner
      model = list(learner = learner$train(task))

      if (isTRUE(self$store_bmr)) {
        model$bmr = pe$bmr
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
