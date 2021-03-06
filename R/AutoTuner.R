#' @title AutoTuner
#'
#' @description
#' The `AutoTuner` is a [mlr3::Learner] which wraps another [mlr3::Learner]
#' and performs the following steps during `$train()`:
#'
#' 1. The hyperparameters of the wrapped (inner) learner are trained on the
#'    training data via resampling.
#'    The tuning can be specified by providing a [Tuner], a [bbotk::Terminator],
#'    a search space as [paradox::ParamSet], a [mlr3::Resampling] and a
#'    [mlr3::Measure].
#' 2. The best found hyperparameter configuration is set as hyperparameters
#'    for the wrapped (inner) learner.
#' 3. A final model is fit on the complete training data using the now
#'    parametrized wrapped learner.
#'
#' During `$predict()` the `AutoTuner` just calls the predict method of the
#' wrapped (inner) learner. A set timeout is disabled while fitting the final
#' model.
#'
#' Note that this approach allows to perform nested resampling by passing an
#' [AutoTuner] object to [mlr3::resample()] or [mlr3::benchmark()].
#' To access the inner resampling results, set `store_tuning_instance = TRUE`
#' and execute [mlr3::resample()] or [mlr3::benchmark()] with
#' `store_models = TRUE` (see examples).
#'
#' @template param_store_models
#' @template param_check_values
#' @template param_store_benchmark_result
#'
#' @export
#' @examples
#' task = tsk("pima")
#' train_set = sample(task$nrow, 0.8 * task$nrow)
#' test_set = setdiff(seq_len(task$nrow), train_set)
#' 
#' at = AutoTuner$new(
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 5),
#'   tuner = tnr("random_search"))
#' 
#' # tune hyperparameters and fit final model
#' at$train(task, row_ids = train_set)
#' 
#' # predict with final model
#' at$predict(task, row_ids = test_set)
#' 
#' # show tuning result
#' at$tuning_result
#' 
#' # model slot contains trained learner and tuning instance
#' at$model
#' 
#' # shortcut trained learner
#' at$learner
#' 
#' # shortcut tuning instance
#' at$tuning_instance
#' 
#'
#' ### nested resampling
#' 
#' at = AutoTuner$new(
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 5),
#'   tuner = tnr("random_search"))
#'
#' resampling_outer = rsmp("cv", folds = 3)
#' rr = resample(task, at, resampling_outer, store_models = TRUE)
#'
#' # retrieve inner tuning results.
#' extract_inner_tuning_results(rr)
#' 
#' # performance scores estimated on the outer resampling
#' rr$score()
#' 
#' # unbiased performance of the final model trained on the full data set
#' rr$aggregate()
AutoTuner = R6Class("AutoTuner",
  inherit = Learner,
  public = list(

    #' @field instance_args (`list()`)\cr
    #' All arguments from construction to create the [TuningInstanceSingleCrit].
    instance_args = NULL,

    #' @field tuner ([Tuner]).
    tuner = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param learner ([mlr3::Learner])\cr
    #' Learner to tune, see [TuningInstanceSingleCrit].
    #'
    #' @param resampling ([mlr3::Resampling])\cr
    #' Resampling strategy during tuning, see [TuningInstanceSingleCrit]. This
    #' [mlr3::Resampling] is meant to be the **inner** resampling, operating
    #' on the training set of an arbitrary outer resampling. For this reason
    #' it is not feasible to pass an instantiated [mlr3::Resampling] here.
    #'
    #' @param measure ([mlr3::Measure])\cr
    #' Performance measure to optimize.
    #'
    #' @param search_space ([paradox::ParamSet])\cr
    #' Hyperparameter search space. If `NULL`, the search space is constructed
    #' from the [TuneToken] in the `ParamSet` of the learner.
    #'
    #' @param terminator ([bbotk::Terminator])\cr
    #' When to stop tuning, see [TuningInstanceSingleCrit].
    #'
    #' @param store_tuning_instance (`logical(1)`)\cr
    #' If `TRUE` (default), stores the internally created
    #' [TuningInstanceSingleCrit] with all intermediate results in slot
    #' `$tuning_instance`.
    #'
    #' @param tuner ([Tuner])\cr
    #' Tuning algorithm to run.
    initialize = function(learner, resampling, measure, terminator, tuner,
      search_space = NULL, store_tuning_instance = TRUE,
      store_benchmark_result = TRUE, store_models = FALSE,
      check_values = FALSE) {
      learner = assert_learner(as_learner(learner, clone = TRUE))

      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token")) > 0) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }

      ia = list()
      ia$learner = learner
      ia$resampling = assert_resampling(resampling, instantiated = FALSE)$clone()
      ia$measure = assert_measure(as_measure(measure), learner = learner)
      if(!is.null(search_space)) ia$search_space = assert_param_set(search_space)$clone()
      ia$terminator = assert_terminator(terminator)$clone()

      private$.store_tuning_instance = assert_flag(store_tuning_instance)
      ia$store_benchmark_result = assert_flag(store_benchmark_result)
      ia$store_models = assert_flag(store_models)

      if (!private$.store_tuning_instance && ia$store_benchmark_result) {
        stop("Benchmark results can only be stored if store_tuning_instance is set to TRUE")
      }

      ia$check_values = assert_flag(check_values)
      self$instance_args = ia
      self$tuner = assert_tuner(tuner)$clone()

      super$initialize(
        id = paste0(learner$id, ".tuned"),
        task_type = learner$task_type,
        packages = learner$packages,
        feature_types = learner$feature_types,
        predict_types = learner$predict_types,
        properties = learner$properties
      )

      self$predict_type = learner$predict_type
      self$predict_sets = learner$predict_sets
    },

    #' @description
    #' Extracts the base learner from nested learner objects like
    #' `GraphLearner` in \CRANpkg{mlr3pipelines}. If `recursive = 0`, the (tuned)
    #' learner is returned.
    #'
    #' @param recursive (`integer(1)`)\cr
    #'   Depth of recursion for multiple nested objects.
    #'
    #' @return [Learner].
    base_learner = function(recursive = Inf) {
      if(recursive == 0) self$learner else self$learner$base_learner(recursive -1)
    },

    #' Printer.
    #' @param ... (ignored).
    print = function() {
      search_space = if (is.null(self$instance_args$search_space)) {
        self$instance_args$learner$param_set$search_space()
      } else {
        self$instance_args$search_space
      }
      catf(format(self))
      catf(str_indent("* Model:", if (is.null(self$model)) "-" else class(self$model)[1L]))
      catf("* Search Space:")
      print(search_space)
      catf(str_indent("* Packages:", self$packages))
      catf(str_indent("* Predict Type:", self$predict_type))
      catf(str_indent("* Feature Types:", self$feature_types))
      catf(str_indent("* Properties:", self$properties))
      w = self$warnings
      e = self$errors
      if (length(w)) {
        catf(str_indent("* Warnings:", w))
      }
      if (length(e)) {
        catf(str_indent("* Errors:", e))
      }
    }
  ),

  active = list(

    #' @field archive [ArchiveTuning]\cr
    #' Archive of the [TuningInstanceSingleCrit].
    archive = function() self$tuning_instance$archive,

    #' @field learner ([mlr3::Learner])\cr
    #' Trained learner
    learner = function() {
      # if there is no trained learner, we return the one in instance args
      if (is.null(self$model)) {
        self$instance_args$learner
      } else {
        self$model$learner
      }
    },

    #' @field tuning_instance ([TuningInstanceSingleCrit])\cr
    #' Internally created tuning instance with all intermediate results.
    tuning_instance = function() self$model$tuning_instance,

    #' @field tuning_result (named `list()`)\cr
    #' Short-cut to `result` from [TuningInstanceSingleCrit].
    tuning_result = function() self$tuning_instance$result,

    #' @field predict_type (`character(1)`)\cr
    #' Stores the currently active predict type, e.g. `"response"`.
    #' Must be an element of `$predict_types`.
    predict_type = function(rhs) {
      if (missing(rhs)) {
        return(private$.predict_type)
      }
      if (rhs %nin% self$predict_types) {
        stopf("Learner '%s' does not support predict type '%s'", self$id, rhs)
      }

      # Catches 'Error: Field/Binding is read-only' bug
      tryCatch({
        self$model$learner$predict_type = rhs
      }, error = function(cond){})

      private$.predict_type = rhs
    }
  ),

  private = list(
    .train = function(task) {
      # construct instance from args; then tune
      ia = self$instance_args
      ia$task = task
      instance = do.call(TuningInstanceSingleCrit$new, ia)
      self$tuner$optimize(instance)

      # get learner, set params to optimal, then train we REALLY need to clone
      # here we write to the object and this would change instance_args
      learner = ia$learner$clone(deep = TRUE)
      learner$param_set$values = instance$result_learner_param_vals
      # disable timeout to allow train on full data set without time limit
      # timeout during tuning is not affected
      learner$timeout = c(train = Inf, predict = Inf)
      learner$train(task)

      # the return model is a list of "learner" and "tuning_instance"
      result_model = list()
      result_model$learner = learner

      if (isTRUE(private$.store_tuning_instance)) {
        result_model$tuning_instance = instance
      }
      return(result_model)
    },

    .predict = function(task) {
      self$model$learner$predict(task)
    },

    .store_tuning_instance = NULL
  )
)
