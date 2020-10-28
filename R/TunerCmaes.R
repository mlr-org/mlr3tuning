#' @title TunerCmaes
#'
#' @name mlr_tuners_cmaes
#'
#' @description
#' Subclass that implements CMA-ES calling [adagio::pureCMAES()]
#' from package \CRANpkg{adagio}.
#'
#' @templateVar id cmaes
#' @template section_dictionary_tuners
#' @template section_logging
#'
#' @inheritSection bbotk::OptimizerCmaes Parameters
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' library(data.table)
#' search_space = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' terminator = trm("evals", n_evals = 10)
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = terminator
#' )
#' tt = tnr("cmaes", par = 0.1)
#' # modifies the instance by reference
#' tt$optimize(instance)
#' # returns best configuration and best performance
#' instance$result
#' # allows access of data.table of full path of all evaluations
#' instance$archive
TunerCmaes = R6Class("TunerCmaes",
  inherit = TunerFromOptimizer,
  public = list(

   #' @description
   #' Creates a new instance of this [R6][R6::R6Class] class.
   initialize = function() {
     super$initialize(
       optimizer = OptimizerCmaes$new()
     )
   }
  )
)

mlr_tuners$add("cmaes", TunerCmaes)
