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
#' For the meaning of all other parameters, see [irace::defaultScenario()]. Note
#' that we have removed all control parameters which refer to the termination of
#' the algorithm. Use [TerminatorRunTime] or [TerminatorEvals] instead. Other
#' terminators do not work with `TunerIrace`. We substract 5 seconds from the
#' [TerminatorRunTime] budget for stability reasons.
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
  inherit = Tuner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("n_instances", lower = 1, default = 10),
        ParamInt$new("debugLevel", default = 0, lower = 0),
        ParamInt$new("seed"),
        ParamDbl$new("postselection", default = 0, lower = 0, upper = 1),
        ParamInt$new("elitist", default = 1, lower = 0, upper = 1),
        ParamInt$new("elitistLimit", default = 2, lower = 0),
        ParamInt$new("nbIterations", default = 0, lower = 0),
        ParamInt$new("nbExperimentsPerIteration", default = 0, lower = 0),
        ParamInt$new("minNbSurvival", default = 0, lower = 0),
        ParamInt$new("nbConfigurations", default = 0, lower = 0),
        ParamInt$new("mu", default = 5, lower = 1),
        ParamInt$new("softRestart", default = 1, lower = 0, upper = 1),
        ParamDbl$new("softRestartThreshold"),
        ParamInt$new("digits", default = 4, lower = 1, upper = 15),
        ParamFct$new("testType", default = "F-test",
          levels = c("F-test", "t-test", "t-test-bonferroni", "t-test-holm")),
        ParamInt$new("firstTest", default = 5, lower = 0),
        ParamInt$new("eachTest", default = 1, lower = 1),
        ParamDbl$new("confidence", default = 0.95, lower = 0, upper = 1),
        ParamInt$new("capping", default = 0, lower = 0, upper = 1),
        ParamFct$new("cappingType", default = "median", levels = c("median", "mean", "best", "worst")),
        ParamFct$new("boundType", default = "candidate", levels = c("candidate", "instance")),
        ParamDbl$new("boundMax", default = 0),
        ParamInt$new("boundDigits", default = 0),
        ParamDbl$new("boundPar", default = 1),
        ParamDbl$new("boundAsTimeout", default = 1)
      ))
      ps$values = list(n_instances = 10)

      super$initialize(
        param_set = ps,
        param_classes = c("ParamDbl", "ParamInt", "ParamFct", "ParamLgl"),
        properties = c("dependencies", "single-crit"),
        packages = "irace"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pv = self$param_set$values
      terminator = inst$terminator
      objective = inst$objective

      # Check terminators 
      if (!(inherits(terminator, "TerminatorEvals") || inherits(terminator, "TerminatorRunTime"))) {
        stopf("%s is not supported. Use <TerminatorEvals> or <TerminatorRunTime> instead.", format(inst$terminator))
      }
      
      browser()
      # Set resampling instances
      ri = replicate(pv$n_instances, {
        r = objective$resampling$clone()
        r$instantiate(objective$task)
      })
      pv$n_instances = NULL

      # Make scenario
      scenario = c(list(
        targetRunner = target_runner,
        logFile = tempfile(),
        instances = ri,
        debugLevel = 0,
        maxExperiments = if (inherits(terminator, "TerminatorEvals")) terminator$param_set$values$n_evals else 0,
        maxTime = if (inherits(terminator, "TerminatorRunTime")) terminator$param_set$values$secs - 5 else 0,
        targetRunnerData = list(inst = inst)
      ), pv)

      res = irace::irace(scenario = scenario, parameters = paradox_to_irace(inst$search_space))
      
      # Temporarily store result
      private$.result_id = res$.ID.[1]
    },

    # The final configurations returned by irace are the elites of the final race.
    # We store the best performing one.
    # The reported performance value is the average of all resampling iterations.
    .assign_result = function(inst) {
      if(length(private$.result_id) == 0) {
        stop("irace::irace did not return a result. The evaluated configurations are still accessible through the archive.")
      }
      res = inst$archive$data[get("id_configuration") == private$.result_id, ]
      cols = c(inst$archive$cols_x, "id_configuration")
      xdt = res[1, cols, with = FALSE]
      y = set_names(mean(unlist(res[, inst$archive$cols_y, with = FALSE])), inst$archive$cols_y)
      inst$assign_result(xdt, y)
    },

    .result_id = NULL
  )
)

mlr_tuners$add("irace", TunerIrace)
