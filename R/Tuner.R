#' @title Tuner
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Abstract `Tuner` class that implements the main functionality each tuner must have.
#' A tuner is an object that describes the tuning strategy how to optimize the black-box function and its feasible set
#' defined by the `[PerfEval]` object.
#' The state of tuning is stored in field `pe$bmr`.
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
#' * `tune_result(ties.method = "random")`\cr
#'   (`character(1)`) -> named `list()`\cr
#'   `ties_method` can be "first", "last" or "random".
#'   List with 2 elements:
#'     - `performance` (`numeric()`) with the best performance.
#'     - `values` (named `list()`) with the corresponding hyperparameters values.
#' * `archive(unnest = TRUE)`\cr
#'   (`logical(1)`) -> [data.table::data.table()]\cr
#'   Returns a table of contained resample results, simply delegates to the `archive` method of [PerfEval].
#'
#' @section Technical Details and Subclasses:
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner
#'  * Specify the private abstract method `tune_internal` and use it to call into your optimizer.
#'  * When you set up an objective function, you will call `pe$eval_batch` to evaluate design points.
#'  * The batch-eval is requested at the PerfEval 'pe' object,
#'    so each batch is possibly executed in parallel via [mlr3::benchmark()],
#'    and all evaluations are stored inside of 'pe$bmr'.
#'  * After the batch-eval, all registered terminators are checked, and if one is positive,
#'    an exception is generated of class 'terminated_message'. In this case the current
#'    batch of evals is still stored in pe, but the numeric score are not sent back to
#'    the handling optimizer as it has lost execution control.
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
#' param_set = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' pe = PerfEval$new("iris", "classif.rpart", "holdout", "classif.ce", param_set)
#' terminator = TerminatorEvals$new(3)
#' tt = TunerRandomSearch$new(pe, terminator) # swap this line to use a different Tuner
#' tt$tune()
#' tt$tune_result() # returns best configuration and performance
#' tt$archive() # allows access of data.table / benchmark result of full path of all evaluations
Tuner = R6Class("Tuner",
  public = list(
    settings = NULL,

    initialize = function(settings = list()) {
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

    tune = function() {
      if (is.null(self$pe))
        stopf("PerfEval object is not set yet!")
      # run internal tune function which calls the optimizer
      # the optimizer will call eval_batch,
      # that will generate an exception when terminator is positive
      # we then catch that here and stop
      tryCatch({
        private$tune_internal()
      }, terminated_message = function(cond) {})
      return(invisible(self))
    },

    tune_result = function(ties_method = "random") {
      measures = self$pe$measures
      rr = self$pe$best(ties_method)
      list(performance = rr$aggregate(measures), values = rr$learners[[1L]]$param_set$values)
    },

    archive = function(unnest = TRUE) self$pe$archive(unnest)
  ),

  active = list(
    pe = function(rhs) {
      if (missing(rhs))
        return(private$.pe)
      private$.pe = assert_r6(rhs, "PerfEval")
    }
  ),

  private = list(
    .pe = NULL,

    tune_internal = function() { # every subclass has to implement this to call optimizer
      stop("abstract")
    }
  )
)
