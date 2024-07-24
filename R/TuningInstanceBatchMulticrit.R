#' @title Class for Multi Criteria Tuning
#'
#' @include TuningInstanceBatchSingleCrit.R ArchiveBatchTuning.R
#'
#' @description
#' The [TuningInstanceBatchMultiCrit] specifies a tuning problem for a [Tuner].
#' The function [ti()] creates a [TuningInstanceBatchMultiCrit] and the function [tune()] creates an instance internally.
#'
#' @inherit TuningInstanceBatchSingleCrit details
#'
#' @section Resources:
#' There are several sections about hyperparameter optimization in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#'  * Learn about [multi-objective optimization](https://mlr3book.mlr-org.com/chapters/chapter5/advanced_tuning_methods_and_black_box_optimization.html#sec-multi-metrics-tuning).
#'
#' The [gallery](https://mlr-org.com/gallery-all-optimization.html) features a collection of case studies and demos about optimization.
#'
#' @inheritSection ArchiveBatchTuning Analysis
#'
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measures
#' @template param_terminator
#' @template param_search_space
#' @template param_store_benchmark_result
#' @template param_store_models
#' @template param_check_values
#' @template param_callbacks
#'
#' @template param_internal_search_space
#' @template param_xdt
#' @template param_learner_param_vals
#' @template param_internal_tuned_values
#'
#' @template field_internal_search_space
#'
#' @export
#' @examples
#' # Hyperparameter optimization on the Palmer Penguins data set
#' task = tsk("penguins")
#'
#' # Load learner and set search space
#' learner = lrn("classif.rpart",
#'   cp = to_tune(1e-04, 1e-1, logscale = TRUE)
#' )
#'
#' # Construct tuning instance
#' instance = ti(
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("cv", folds = 3),
#'   measures = msrs(c("classif.ce", "time_train")),
#'   terminator = trm("evals", n_evals = 4)
#' )
#'
#' # Choose optimization algorithm
#' tuner = tnr("random_search", batch_size = 2)
#'
#' # Run tuning
#' tuner$optimize(instance)
#'
#' # Optimal hyperparameter configurations
#' instance$result
#'
#' # Inspect all evaluated configurations
#' as.data.table(instance$archive)
TuningInstanceBatchMultiCrit = R6Class("TuningInstanceBatchMultiCrit",
  inherit = OptimInstanceBatchMultiCrit,

  public = list(

    internal_search_space = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      task,
      learner,
      resampling,
      measures,
      terminator,
      search_space = NULL,
      store_benchmark_result = TRUE,
      store_models = FALSE,
      check_values = FALSE,
      callbacks = NULL
      ) {
      learner = assert_learner(as_learner(learner, clone = TRUE))

      # tune token and search space
      if (!is.null(search_space) && length(learner$param_set$get_values(type = "only_token"))) {
        stop("If the values of the ParamSet of the Learner contain TuneTokens you cannot supply a search_space.")
      }
      if (is.null(search_space)) {
        search_space = as_search_space(learner)
        learner$param_set$values = learner$param_set$get_values(type = "without_token")
      } else {
        search_space = as_search_space(search_space)
      }

      # internal search space
      internal_tune_ids = keep(names(search_space$tags), map_lgl(search_space$tags, function(tag) "internal_tuning" %in% tag))
      if (length(internal_tune_ids)) {
        self$internal_search_space = search_space$subset(internal_tune_ids)

        if (self$internal_search_space$has_trafo) {
          stopf("Inner tuning and parameter transformations are currently not supported.")
        }

        search_space = search_space$subset(setdiff(search_space$ids(), internal_tune_ids))

        # the learner dictates how to interpret the to_tune(..., inner)
        learner$param_set$set_values(.values = learner$param_set$convert_internal_search_space(self$internal_search_space))
      }

      # create codomain from measure
      measures = assert_measures(as_measures(measures, task_type = task$task_type), task = task, learner = learner)
      codomain = measures_to_codomain(measures)

      # initialized specialized tuning archive and objective
      archive = ArchiveBatchTuning$new(
        search_space = search_space,
        codomain = codomain,
        check_values = check_values,
        internal_search_space = self$internal_search_space
      )

      objective = ObjectiveTuningBatch$new(
        task = task,
        learner = learner,
        resampling = resampling,
        measures = measures,
        store_benchmark_result = store_benchmark_result,
        store_models = store_models,
        check_values =  check_values,
        archive = archive,
        callbacks = callbacks,
        internal_search_space = self$internal_search_space)

      super$initialize(
        objective = objective,
        search_space = search_space,
        terminator = terminator,
        callbacks = callbacks,
        archive = archive)
    },

    #' @description
    #' The [Tuner] object writes the best found points and estimated performance values here.
    #' For internal use.
    #'
    #' @param ydt (`data.table::data.table()`)\cr
    #'   Optimal outcomes, e.g. the Pareto front.
    #' @param ... (`any`)\cr
    #' ignored.
    assign_result = function(xdt, ydt, learner_param_vals = NULL) {
      # set the column with the learner param_vals that were not optimized over but set implicitly
      if (is.null(learner_param_vals)) {
        learner_param_vals = self$objective$learner$param_set$values
        if (length(learner_param_vals) == 0) learner_param_vals = list()
        learner_param_vals = replicate(nrow(ydt), list(learner_param_vals))
      }

      opt_x = transform_xdt_to_xss(xdt, self$search_space)
      if (length(opt_x) == 0) opt_x = replicate(length(ydt), list())
      learner_param_vals = Map(insert_named, learner_param_vals, opt_x)

      # disable internal tuning
      if (!is.null(xdt$internal_tuned_values)) {
        learner = self$objective$learner$clone(deep = TRUE)
        learner_param_vals = pmap(list(learner_param_vals, xdt$internal_tuned_values), function(lpv, itv) {
          values = insert_named(lpv, itv)
          learner$param_set$set_values(.values = values, .insert = FALSE)
          learner$param_set$disable_internal_tuning(self$internal_search_space$ids())
          learner$param_set$values
        })
      }

      set(xdt, j = "learner_param_vals", value = list(learner_param_vals))

      super$assign_result(xdt, ydt)
    }
  ),

  active = list(
    #' @field result_learner_param_vals (`list()`)\cr
    #'   List of param values for the optimal learner call.
    result_learner_param_vals = function() {
      private$.result$learner_param_vals

    }
  ),

  private = list(
    # initialize context for optimization
    .initialize_context = function(optimizer) {
      context = ContextBatchTuning$new(self, optimizer)
      self$objective$context = context
    }
  )
)
