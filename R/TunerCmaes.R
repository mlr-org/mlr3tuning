#' @title Hyperparameter Tuning with Covariance Matrix Adaptation Evolution Strategy
#'
#' @name mlr_tuners_cmaes
#'
#' @description
#' Subclass that implements CMA-ES calling [adagio::pureCMAES()]
#' from package \CRANpkg{adagio}.
#'
#' @templateVar id cmaes
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`sigma`}{`numeric(1)`}
#' \item{`start_values`}{`character(1)`\cr
#' Create `random` start values or based on `center` of search space? In the 
#' latter case, it is the center of the parameters before a trafo is applied.}
#' }
#'
#' For the meaning of the control parameters, see [adagio::pureCMAES()]. Note
#' that we have removed all control parameters which refer to the termination of
#' the algorithm and where our terminators allow to obtain the same behavior.
#' 
#' @template section_progress_bars
#' @template section_logging
#' 
#' @source 
#' `r format_bib("hansen_2016")`
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @examples
#' library(data.table)
#' 
#' # retrieve task
#' task = tsk("pima")
#' 
#' # load learner and set search space
#' learner = lrn("classif.rpart", 
#'   cp = to_tune(1e-04, 1e-1, logscale = TRUE), 
#'   minsplit = to_tune(p_dbl(2, 128, trafo = as.integer)),
#'   minbucket = to_tune(p_dbl(1, 64, trafo = as.integer))
#' )
#' 
#' # hyperparameter tuning on the pima indians diabetes data set
#' instance = tune(
#'   method = "cmaes",
#'   task = task,
#'   learner = learner,
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   term_evals = 10)
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
