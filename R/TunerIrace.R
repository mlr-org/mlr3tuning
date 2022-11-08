#' @title Hyperparameter Tuning with Iterated Racing.
#'
#' @include Tuner.R
#' @name mlr_tuners_irace
#'
#' @description
#' Subclass for iterated racing.
#' Calls [irace::irace()] from package \CRANpkg{irace}.
#'
#' @templateVar id irace
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`n_instances`}{`integer(1)`\cr
#' Number of resampling instances.}
#' }
#'
#' For the meaning of all other parameters, see [irace::defaultScenario()]. Note
#' that we have removed all control parameters which refer to the termination of
#' the algorithm. Use [TerminatorEvals] instead. Other terminators do not work
#' with `TunerIrace`.
#'
#' @section Archive:
#' The [ArchiveTuning] holds the following additional columns:
#'  * `"race"` (`integer(1)`)\cr
#'    Race iteration.
#'  * `"step"` (`integer(1)`)\cr
#'    Step number of race.
#'  * `"instance"` (`integer(1)`)\cr
#'    Identifies resampling instances across races and steps.
#'  * `"configuration"` (`integer(1)`)\cr
#'    Identifies configurations across races and steps.
#'
#' @section Result:
#' The tuning result (`instance$result`) is the best performing elite of
#' the final race. The reported performance is the average performance estimated
#' on all used instances.
#'
#' @template section_progress_bars
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerIrace
#' @template section_optimizer
#'
#' @source
#' `r format_bib("lopez_2016")`
#'
#' @family Tuner
#' @export
#' @examples
#' # retrieve task
#' task = tsk("pima")
#'
#' # load learner and set search space
#' learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE))
#' \donttest{
#' # hyperparameter tuning on the pima indians diabetes data set
#' instance = tune(
#'   method = "irace",
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 42
#' )
#'
#' # best performing hyperparameter configuration
#' instance$result
#'
#' # all evaluated hyperparameter configuration
#' as.data.table(instance$archive)
#'
#' # fit final model on complete data set
#' learner$param_set$values = instance$result_learner_param_vals
#' learner$train(task)
#' }
TunerIrace = R6Class("TunerIrace",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      optimizer = OptimizerIrace$new()
      optimizer$param_set$add(ParamInt$new("n_instances", lower = 1, default = 10))
      optimizer$param_set$set_values(
        n_instances = 10,
        targetRunnerParallel = target_runner_tuning
      )

      super$initialize(
        optimizer = optimizer,
        man = "mlr3tuning::mlr_tuners_irace"
      )
    },

    #' @description
    #' Performs the tuning on a [TuningInstanceSingleCrit] until termination.
    #' The single evaluations and the final results will be written into the
    #' [ArchiveTuning] that resides in the [TuningInstanceSingleCrit]. The final
    #' result is returned.
    #'
    #' @param inst ([TuningInstanceSingleCrit]).
    #'
    #' @return [data.table::data.table].
    optimize = function(inst) {
      assert_class(inst, "TuningInstanceSingleCrit")
      n_instances = private$.optimizer$param_set$values$n_instances

      # Set resampling instances
      ri = replicate(n_instances, {
        r = inst$objective$resampling$clone()
        r$instantiate(inst$objective$task)
      })
      private$.optimizer$param_set$values$instances = ri

      # temporary remove n_instance from parameter set values
      private$.optimizer$param_set$values$n_instances = NULL

      private$.optimizer$optimize(inst)

      # restore n_instances in parameter set
      private$.optimizer$param_set$values$n_instances = n_instances

      return(inst$result)
    }
  )
)

target_runner_tuning = function(experiment, exec.target.runner, scenario, target.runner) {# nolint
  tuning_instance = scenario$targetRunnerData$inst

  xdt = map_dtr(experiment, function(e) {
    configuration = as.data.table(e$configuration)
    # add configuration and instance id to archive
    set(configuration, j = "configuration", value = e$id.configuration)
    set(configuration, j = "instance", value = e$id.instance)
    # fix logicals
    configuration[, map(.SD, function(x) ifelse(x %in% c("TRUE", "FALSE"), as.logical(x), x))]
  })

  # provide experiment instances to objective
  tuning_instance$objective$constants$values$resampling = map(experiment, function(e) e$instance)

  # evaluate configuration
  res = tuning_instance$eval_batch(xdt)

  # return cost (minimize) and dummy time to irace
  map(transpose_list(res), function(cost) {
    list(cost = unlist(cost) * tuning_instance$objective_multiplicator, time = NA_real_)
  })
}

mlr_tuners$add("irace", TunerIrace)
