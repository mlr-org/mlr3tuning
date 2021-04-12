#' @title Tuning via Iterated Racing.
#'
#' @include Tuner.R
#' @name mlr_tuners_irace
#'
#' @description
#' `TunerIrace` class that implements iterated racing. Calls [irace::irace()]
#' from package \CRANpkg{irace}.
#'
#' @section Parameters:
#' \describe{
#' \item{`n_instances`}{`integer(1)`\cr
#' Number of resampling instances.}
#' }
#'
#' For the meaning of all other parameters, see [irace::defaultScenario()]. The
#' parameter `instances` and `targetRunner` are automatically set by the tuner.
#' Note that we have removed all control parameters which refer to the
#' termination of the algorithm. Use [TerminatorRunTime] or [TerminatorEvals]
#' instead. Other terminators do not work with `TunerIrace`. We substract 5
#' seconds from the [TerminatorRunTime] budget for stability reasons.
#'
#' @templateVar id irace
#' @template section_dictionary_tuners
#'
#' @source
#' `r format_bib("lopez_2016")`
#'
#' @family Tuner
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' search_space = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' terminator = trm("evals", n_evals = 42)
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = terminator
#' )
#' tt = tnr("irace")
#'
#' # modifies the instance by reference
#' tt$optimize(instance)
#'
#' # returns best configuration and best performance
#' instance$result
#'
#' # allows access of data.table of full path of all evaluations
#' instance$archive
TunerIrace = R6Class("TunerIrace",
  inherit = TunerFromOptimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      optimizer = OptimizerIrace$new()
      optimizer$param_set$add(ParamInt$new("n_instances", lower = 1, default = 10))
      optimizer$param_set$values = list(n_instances = 10, targetRunner = target_runner)

      super$initialize(optimizer = optimizer)
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

target_runner = function(experiment, scenario) { # nolint
  t0 = Sys.time()
  tuning_instance = scenario$targetRunnerData$inst

  # fix logicals
  config = as.data.table(lapply(experiment$configuration, function(x) {
    if (x %in% c("TRUE", "FALSE")) {
      return(as.logical(x))
    } else {
      return(x)
    }
  }))

  # change resampling instance
  tuning_instance$objective$resampling = experiment$instance

  # add extra info to archive
  extra = data.table(id_configuration = experiment$id.configuration, id_instance = experiment$id.instance)

  # evaluate configuration
  # objective_function cannot pass extra information
  cost = as.numeric(tuning_instance$eval_batch(cbind(config, extra))) * tuning_instance$objective_multiplicator

  return(list(cost = cost, time = as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

mlr_tuners$add("irace", TunerIrace)
