#' @title AutoTuner
#'
#' @description
#' The `AutoTuner` is a [mlr3::Learner] which auto-tunes by first tuning the
#' hyperparameters of its encapsulated learner on the training data, then
#' setting the optimal configuration in the learner, then finally fitting the
#' model on the complete training data. This class allows to perform nested
#' resampling by passing an [AutoTuner] object to [mlr3::resample()] or
#' [mlr3::benchmark()].
#'
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' task = tsk("iris")
#' learner = lrn("classif.rpart")
#' resampling = rsmp("holdout")
#' measure = msr("classif.ce")
#' search_space = ParamSet$new(
#'   params = list(ParamDbl$new("cp", lower = 0.001, upper = 0.1)))
#'
#' terminator = term("evals", n_evals = 5)
#' tuner = tnr("grid_search")
#' at = AutoTuner$new(
#'   learner, resampling, measure, search_space, terminator,
#'   tuner)
#' at$store_tuning_instance = TRUE
#'
#' at$train(task)
#' at$model
#' at$learner
AutoTuner = R6Class("AutoTuner",
  inherit = Learner,
  public = list(

    #' @field instance_args (`list()`)\cr
    #' All arguments from construction to create the [TuningInstance].
    instance_args = NULL,

    #' @field tuner ([Tuner]).
    tuner = NULL,

    #' @field store_tuning_instance (`logical(1)`)\cr
    #' If `TRUE` (default), stores the internally created [TuningInstance]
    #' with all intermediate results in slot `$tuning_instance`.
    store_tuning_instance = TRUE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::Learner])\cr
    #' Learner to tune, see [TuningInstance].
    #'
    #' @param resampling ([mlr3::Resampling])\cr
    #' Resampling strategy during tuning, see [TuningInstance]. This
    #' [mlr3::Resampling] is meant to be the **inner** resampling, operating
    #' on the training set of an arbitrary outer resampling. For this reason
    #' it is not feasible to pass an instantiated [mlr3::Resampling] here.
    #'
    #' @param measure (list of [mlr3::Measure])\cr
    #' Performance measure to optimize.
    #'
    #' @param search_space ([paradox::ParamSet])\cr
    #' Hyperparameter search space, see [TuningInstance].
    #'
    #' @param terminator ([Terminator])\cr
    #' When to stop tuning, see [TuningInstance].
    #'
    #' @param tuner ([Tuner])\cr
    #' Tuning algorithm to run.
    #'
    #' @param bm_args (named `list()`)\cr
    #' Further arguments for [mlr3::benchmark()], see [TuningInstance].
    initialize = function(learner, resampling, measure, search_space,
      terminator, tuner) {
      ia = list()
      ia$learner = assert_learner(learner)$clone(deep = TRUE)
      ia$resampling = assert_resampling(resampling,
        instantiated = FALSE)$clone()
      ia$measure = assert_measure(as_measure(measure), learner = learner)
      ia$search_space = assert_param_set(search_space)$clone()
      # FIXME: i have no idea why we do this here?
      ia$learner$param_set$set_id = ""
      ia$terminator = assert_terminator(terminator)$clone()
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
    }
  ),

  active = list(

    #' @field archive Returns TuningInstance archive
    archive = function() self$tuning_instance$archive,

    #' @field learner ([mlr3::Learner])\cr
    #'   Trained learner
    learner = function() {
      # if there is no trained learner, we return the one in instance args
      if (is.null(self$model)) {
        self$instance_args$learner
      } else {
        self$model$learner
      }
    },

    #' @field tuning_instance ([TuningInstance])\cr
    #'   Internally created tuning instance with all intermediate results.
    tuning_instance = function() self$model$tuning_instance,

    #' @field tuning_result (named `list()`)\cr
    #'   Short-cut to `result` from [TuningInstance].
    tuning_result = function() self$tuning_instance$result,

    #' @field param_set (paradox::ParamSet].
    param_set = function(rhs) {
      if (is.null(private$.param_set)) {
        private$.param_set = ParamSetCollection$new(list(
          # --> this is how we would insert the self$tuner_paramset:
          # self$tuner$param_set,
          self$learner$param_set
        ))
        private$.param_set$set_id = private$.ps_id
      }

      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),

  private = list(
    .train = function(task) {
      # construct instance from args; then tune
      ia = self$instance_args
      ia$task = task
      instance = do.call(TuningInstance$new, ia)
      self$tuner$optimize(instance)

      # get learner, set params to optimal, then train we REALLY need to clone
      # here we write to the object and this would change instance_args
      learner = ia$learner$clone(deep = TRUE)
      learner$param_set$values = instance$result_learner_param_vals
      learner$train(task)

      # the return model is a list of "learner" and "tuning_instance"
      result_model = list()
      result_model$learner = learner
      if (isTRUE(self$store_tuning_instance)) {
        result_model$tuning_instance = instance
      }
      return(result_model)
    },

    .predict = function(task) {
      self$model$learner$predict(task)
    },

    deep_clone = function(name, value) {
      if (!is.null(private$.param_set)) {
        private$.ps_id = private$.param_set$set_id
        # required to keep clone identical to original, otherwise tests get
        # really ugly
        private$.param_set = NULL
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    .ps_id = ""
  )
)
