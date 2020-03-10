#' @title TunerIrace
#'
#' @name mlr_tuners_irace
#' @include Tuner.R
#'
#' @description
#' Subclass for iterated racing tuning calling [irace::irace()] from package \CRANpkg{irace}.
#'
#' @section Parameters:
#' * `show.irace.output` (`logical(1)`)
#' * `debugLevel` (`integer(1)`)
#' * `seed` (`integer(1)`)
#' * `postselection` (`numeric(1)`)
#' * `elitist` (`integer(1)`)
#' * `elitistLimit` (`integer(1)`)
#' * `nbIterations` (`integer(1)`)
#' * `nbExperimentsPerIteration` (`integer(1)`)
#' * `minNbSurvival` (`integer(1)`)
#' * `nbConfigurations` (`integer(1)`)
#' * `mu` (`integer(1)`)
#' * `softRestart` (`integer(1)`)
#' * `softRestartThreshold` (`numeric(1)`)
#' * `digits` (`integer(1)`)
#' * `testType` (`character(1)`)
#' * `firstTest` (`integer(1)`)
#' * `eachTest` (`integer(1)`)
#' * `confidence` (`numeric(1)`)
#' * `capping` (`integer(1)`)
#' * `cappingType` (`character(1)`)
#' * `boundType` (`character(1)`)
#' * `boundMax` (`numeric(1)`)
#' * `boundDigits` (`integer(1)`)
#' * `boundPar` (`numeric(1)`)
#' * `boundAsTimeout` (`numeric(1)`)
#'
#' `show.irace.output` suppresses the output from [irace::irace()] and only prints output from this package.
#' For the meaning of all other parameters, see [irace::defaultScenario()].
#' Note that we have removed all control parameters which refer to the termination of the algorithm and
#' where our terminators allow to obtain the same behavior.
#'
#' @templateVar id irace
#' @template section_dictionary_tuners
#'
#' @references
#' Birattari, M., Stützle, T., Paquete, L., & Varrentrapp, K. (2002).
#' A Racing Algorithm for Configuring Metaheuristics.
#' In Gecco (Vol. 2).
#'
#' @family Tuner
#' @export
#' @examples
#' # see ?Tuner
TunerIrace = R6Class("TunerIrace", inherit = Tuner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamLgl$new("show.irace.output", default = FALSE),
        ParamInt$new("debugLevel", default = 0, lower = 0),
        ParamInt$new("seed"),
        ParamDbl$new("postselection",default=0,lower=0,upper=1),
        ParamInt$new("elitist",default=1,lower=0,upper=1),
        ParamInt$new("elitistLimit",default=2,lower=0),
        ParamInt$new("nbIterations",default=0,lower=0),
        ParamInt$new("nbExperimentsPerIteration",default=0,lower=0),
        ParamInt$new("minNbSurvival",default=0,lower=0),
        ParamInt$new("nbConfigurations",default=0,lower=0),
        ParamInt$new("mu",default=5,lower=1),
        ParamInt$new("softRestart",default=1,lower=0,upper=1),
        ParamDbl$new("softRestartThreshold"),
        ParamInt$new("digits", default = 4, lower = 1, upper = 15),
        ParamFct$new("testType", default = "F-test", levels = c("F-test","t-test","t-test-bonferroni","t-test-holm")),
        ParamInt$new("firstTest", default = 5, lower = 0),
        ParamInt$new("eachTest",default = 1, lower = 1),
        ParamDbl$new("confidence",default=0.95,lower=0,upper=1),
        ParamInt$new("capping",default = 0,lower=0,upper=1),
        ParamFct$new("cappingType",default="median",levels=c("median","mean","best","worst")),
        ParamFct$new("boundType",default="candidate",levels=c("candidate","instance")),
        ParamDbl$new("boundMax",default=0),
        ParamInt$new("boundDigits",default=0),
        ParamDbl$new("boundPar",default=1),
        ParamDbl$new("boundAsTimeout",default=1)
      ))

      ps$values$show.irace.output = FALSE

      super$initialize(
        param_set = ps,
        param_classes = c("ParamDbl","ParamInt","ParamFct","ParamLgl"),
        properties = "dependencies",
        packages = "irace"
      )
    }
  ),

  private = list(
    .tune = function(instance) {
      g = if (self$param_set$values$show.irace.output) identity else capture.output
      g(irace::irace(scenario = c(make_scenario(instance),
                                  self$param_set$values[names(self$param_set$values) %nin% "show.irace.output"]),
                     parameters = paradox_to_irace(instance$param_set)))
    }
  )
)

mlr_tuners$add("irace", TunerIrace)