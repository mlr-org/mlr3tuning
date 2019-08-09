#' @title Tuner
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Abstract `Tuner` class that implements the main functionality each tuner must have.
#' A tuner is an object that describes the tuning strategy how to search the hyperparameter space given within
#' the `[PerfEval]` object.
#' The state of tuning is stored in field `pe$bmr` and the tuner offers some active
#' bindings to conveniently access results.
#'
#' @section Construction:
#' ```
#' tuner = Tuner$new(pe, terminator, settings = list())
#' ```
#' * `pe` :: [PerfEval].
#' * `terminator` :: [Terminator].
#' * `settings` :: named `list()`\cr
#'   Arbitrary list, depending on the child class.
#'
#' @section Fields:
#' * `pe` :: [PerfEval]\cr
#'   The current state of the [PerfEval].
#' * `terminator` :: [Terminator].
#'   The current state of the [Terminator].
#' * `settings` :: named `list()`\cr
#'   Arbitrary list, depending on the child class.
#'
#' @section Methods:
#' * `tune()`\cr
#'   () -> `self`\cr
#'   Performs the tuning until the [Terminator] becomes positive.
#' * `tune_result()`\cr
#'   () -> named `list()`\cr
#'   List with 2 elements:
#'     - `performance` (`numeric()`) with the best performance.
#'     - `values` (named `list()`) with the corresponding hyperparameters values.
#' * `archive(unnest = TRUE)`\cr
#'   (`logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a table of contained resample results, simply delegates to the `archive` method of [PerfEval].
#' * `eval_batch(dt)`\cr
#'   ([data.table::data.table()]) -> `numeric(n)`\cr
#'   Evaluates a set of design points, passed as a data.table of n rows and returns n performance values.
#'   This is the only entry point a subclass tuner should use to evaluate the objective function,
#'   and it should normally not be called from the outside by the user.
#'   The difference to `eval_batch` in [PerfEval] is that this also runs all associated terminators and potentially
#'   stops with an exception.
#'
#' @section Technical Details and Subclasses:
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner
#'  * Specify the private abstract method `tune_interal` and use it to call into your optimizer.
#'  * When you set up an objective function, you will call `eval_batch` to evaluate design points.
#'  * The batch-eval is requested at the PerfEval 'pe' object,
#'    so each batch is possibly executed in parallel via [mlr3::benchmark()],
#'    and all evaluations are stored inside of 'pe$bmr'.
#'  * After the batch-eval, all terminators registered with the tuner are checked, and if one is positive,
#'    an exception is generated of class 'terminated_message'. In this case the current
#'    batch of evals is still stored in pe, but the numeric score are not sent back to t
#'    he handling optimizer as it has lost execution control.
#'  * After such an exception was caught we select the best configuration from `pe$bmr` and
#'    return it.
#'  * Note that therefore more points than specified by the Terminator might be evaluated,
#'    as the Terminator is only checked after a batch-eval. How many more depends on the batchsize.
#'
#' @family Tuner
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' resampling = mlr_resamplings$get("cv", param_vals = list(folds = 2))
#' param_set = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' pe = PerfEval$new("iris", "classif.rpart", resampling, "classif.ce", param_set)
#' terminator = TerminatorEvaluations$new(3)
#' tt = TunerRandomSearch$new(pe, terminator) # swap this line to use a different Tuner
#' tt$tune()
#' tt$tune_result() # returns best configuration and performance
#' tt$archive() # allows access of data.table / benchmark result of full path of all evaluations
Tuner = R6Class("Tuner",
  public = list(
    pe = NULL,
    terminator = NULL,
    settings = NULL,

    initialize = function(pe, terminator, settings = list()) {
      self$pe = assert_r6(pe, "PerfEval")
      self$terminator = assert_r6(terminator, "Terminator")
      self$settings = assert_list(settings, names = "unique")
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(format(self))
      catf(str_indent("* Terminator:", format(self$terminator)))
      catf(str_indent("* settings:", as_short_string(self$settings)))
      catf(str_indent("* PerfEval:", format(self$pe)))
      print(self$pe)
    },

    eval_batch = function(dt) {
      lg$info("Evaluating %i configurations", nrow(dt))
      self$terminator$eval_before(self$pe)
      self$pe$eval_batch(dt)
      self$terminator$eval_after(self$pe)

      # if he is terminated throw condition of class "terminated_message" that we can tryCatch.
      # if the terminator is terminated.
      if (self$terminator$terminated) {
        stop(messageCondition("Termination criteria is reached", class = "terminated_message"))
      }
    },

    tune = function() {
      # run internal tune function which calls the optimizer
      # the optimizer will call eval_batch,
      # that will generate an exception when terminator is positive
      # we then catch that here and stop
      tryCatch({
        private$tune_internal()
      }, terminated_message = function(cond) {})
      return(invisible(self))
    },

    tune_result = function() {
      measures = self$pe$measures
      rr = self$pe$bmr$best(measures[[1L]])
      list(performance = rr$aggregate(measures), values = rr$learners[[1L]]$param_set$values)
    },

    archive = function(unnest = TRUE) self$pe$archive(unnest)
  ),

  private = list(
    tune_internal = function() { # every subclass has to implement this to call optimizer
      stop("abstract")
    }
  )
)
