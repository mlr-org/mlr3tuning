#' @title TunerRandomSearch
#'
#' @description
#' TunerRandomSearch
#'
#' @section Usage:
#' ```
#' l = TunerRandomSearch(id)
#' # public members
#' # public methods
#' # active bindings
#' ```
#'
#' @section Arguments:
#' * `id` (`character(1)`):
#'   The id of the Tuner.
#'
#' @section Details:
#' `$new()` creates a new object of class [TunerRandomSearch].
#'
#' @name TunerRandomSearch
#' @keywords internal
#' @family Tuner
NULL

#' @export
TunerRandomSearch = R6Class("TunerRandomSearch",
  inherit = TunerBase,
  public = list(
  
    initialize = function(id, terminator, fitness_function_class = NULL, batch_n = 100) {
      super$initialize(id = id, settings = list(batch_n = batch_n), terminator = terminator, fitness_function_class = fitness_function_class)
      #FIXME: Allow initialization with concrete fitness_function?
    },

    tune = function(task, learner, param_set) {
      self$fitness_function = self$fitness_function_class$new(
        task = task, learner = learner, resampling = self$resampling, param_set = param_set, terminator = self$terminator)
      if ("TerminatorEvaluations" %in% class(self$terminator)) {
        self$settings$batch_n = self$fitness_function$terminator$settings$max_evaluations
      }
      terminated = FALSE
      while (!terminated) {
        self$tune_step()  
        terminated = self$fitness_function$terminator$terminated
      }
    },

    tune_step = function() {
      xs = self$fitness_function$param_set$sample(self$settings$batch_n)
      xs = self$fitness_function$param_set$transform(xs)
      y = self$fitness_function$eval_vectorized(xs)
      invisible(y)
    }
    
  ),
  active = list(),
  private = list()
)