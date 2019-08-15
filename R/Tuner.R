#' @title Tuner
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#'
#' @description
#' Abstract `Tuner` class that implements the main functionality each tuner must have.
#' A tuner is an object that describes the tuning strategy how to optimize the black-box function and its feasible set
#' defined by the `[PerfEval]` object.
#'
#' @section Construction:
#' ```
#' tuner = Tuner$new(settings = list())
#' ```
#' * `settings` :: named `list()`\cr
#'   Arbitrary list, depending on the child class.
#' * `param_classes` :: `character`\cr
#'   Supported parameter classes that the tuner can optimize, subclasses of [paradox::Param].
#'
#' @section Fields:
#' * `settings` :: named `list()`\cr
#'
#' @section Methods:
#' * `tune(pe)`\cr
#'   ([PerfEval]) -> `list`\cr
#'   Performs the tuning on a [PerfEval] until termination.
#'   Returns list with 2 elements:
#'     - `performance` (`numeric()`) with the best performance.
#'     - `values` (named `list()`) with the corresponding hyperparameters values.
#'
#' @section Technical Details and Subclasses:
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner
#'  * Specify the private abstract method `tune_internal` and use it to call into your optimizer.
#'  * When you set up an objective function, you will call `pe$eval_batch` to evaluate design points.
#'  * The batch-eval is requested at the PerfEval 'pe' object,
#'    so each batch is possibly executed in parallel via [mlr3::benchmark()],
#'    and all evaluations are stored inside of 'pe$bmr'.
#'  * After the batch-eval, the terminator is checked, and if is positive,
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
#' terminator = TerminatorEvals$new(3)
#' pe = PerfEval$new("iris", "classif.rpart", "holdout", "classif.ce", param_set, terminator)
#' tt = TunerRandomSearch$new() # swap this line to use a different Tuner
#' res = tt$tune(pe) # returns best configuration and performance, and logs in 'pe'
#' pe$archive() # allows access of data.table / benchmark result of full path of all evaluations
Tuner = R6Class("Tuner",
  public = list(
    settings = NULL,
    param_classes = NULL,
    ties_method = "random", # FIXME: bad handling

    initialize = function(param_classes, settings = list()) {
      self$param_classes = param_classes
      self$settings = assert_list(settings, names = "unique")
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(format(self))
      catf(str_indent("* settings:", as_short_string(self$settings)))
    },

    tune = function(pe) {
      not_supported_pclasses = setdiff(unique(pe$param_set$class), self$param_classes)
      if (length(not_supported_pclasses) > 0L)
        stopf("Tuner '%s' does not support param types: '%s'", class(self)[1L], paste0(not_supported_pclasses, collapse = ","))
      pe$start_time = Sys.time()
      # run internal tune function which calls the optimizer
      # the optimizer will call eval_batch,
      # that will generate an exception when terminator is positive
      # we then catch that here and stop
      tryCatch({
        private$tune_internal(pe)
      }, terminated_message = function(cond) {
      })

      rr = pe$best(self$ties_method)
      # FIXME: autotuner setting later
      list(performance = rr$aggregate(pe$measures), values = rr$learners[[1L]]$param_set$values)
    }
  ),

  private = list(
    tune_internal = function(pe) {
      # every subclass has to implement this to call optimizer
      stop("abstract")
    }
  )
)
