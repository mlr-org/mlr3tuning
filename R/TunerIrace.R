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
        ParamLgl$new("show.irace.output", default = FALSE, tags = c("meta","required")),
        ParamInt$new("debugLevel", default = 0, lower = 0, tags = "irace"),
        ParamInt$new("seed", tags = "irace"),
        ParamDbl$new("postselection",default=0,lower=0,upper=1, tags = "irace"),
        ParamInt$new("elitist",default=1,lower=0,upper=1, tags = "irace"),
        ParamInt$new("elitistLimit",default=2,lower=0, tags = "irace"),
        ParamInt$new("mu",default=5,lower=1, tags = "irace"),
        ParamInt$new("softRestart",default=1,lower=0,upper=1, tags = "irace"),
        ParamDbl$new("softRestartThreshold", tags = "irace"),
        ParamInt$new("digits", default = 4, lower = 0, tags = "irace"),
        ParamFct$new("testType", default = "F-test", levels = c("F-test","t-test","t-test-bonferroni","t-test-holm"), tags = "irace"),
        ParamInt$new("firstTest", default = 5, lower = 0, tags = "irace"),
        ParamInt$new("eachTest",default = 1, lower = 1, tags = "irace"),
        ParamDbl$new("confidence",default=0.95,lower=0,upper=1, tags = "irace"),
        ParamInt$new("capping",default = 0,lower=0,upper=1, tags = "irace"),
        ParamFct$new("cappingType",default="median",levels=c("median","mean","best","worst"), tags = "irace"),
        ParamFct$new("boundType",default="candidate",levels=c("candidate","instance"), tags = "irace"),
        ParamDbl$new("boundMax",default=0, tags = "irace"),
        ParamInt$new("boundDigits",default=0, tags = "irace"),
        ParamDbl$new("boundPar",default=1, tags = "irace"),
        ParamDbl$new("boundAsTimeout",default=1, tags = "irace")
      ))

      ps$values$show.irace.output = FALSE

      super$initialize(
        param_set = ps,
        param_classes = c("ParamDbl","ParamInt","ParamFct","ParamLgl"),
        properties = character(),
        packages = "irace"
      )
    }
  ),

  private = list(
    .tune = function(instance) {
      g = if (self$param_set$get_values(tag = "meta")$show.irace.output) identity else capture.output
      g(irace::irace(scenario = c(makeScenario(instance), self$param_set$get_values("irace")),
                     parameters = paradoxToIrace(instance$param_set)))
    }
  )
)

mlr_tuners$add("irace", TunerIrace)
