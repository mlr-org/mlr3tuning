#' @title Tuner
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_tuners.R
#'
#' @description
#' Abstract `Tuner` class that implements the main functionality each tuner must have.
#' A tuner is an object that describes the tuning strategy, i.e. how to optimize the black-box
#' function and its feasible set defined by the [TuningInstance] object.
#'
#' @section Construction:
#' ```
#' tuner = Tuner$new(param_classes, settings = list())
#' ```
#'
#' * `param_classes` :: `character()`\cr
#'   Supported parameter classes that the tuner can optimize, subclasses of [paradox::Param].
#' * `settings` :: named `list()`\cr
#'   Arbitrary named list, depending on the child class.
#'
#' @section Fields:
#' * `settings` :: named `list()`\cr
#'
#' @section Methods:
#' * `tune(instance)`\cr
#'   ([TuningInstance]) -> `list()`\cr
#'   Performs the tuning on a [TuningInstance] until termination.
#'   Returns a list with 2 elements:
#'     - `performance` (`numeric()`) with the best performance.
#'     - `values` (named `list()`) with the corresponding hyperparameters values.
#'
#' @section Technical Details and Subclasses:
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner
#'  * Specify the private abstract method `$tune_internal()` and use it to call into your optimizer.
#'  * When you set up an objective function, you will call `instance$eval_batch()` to evaluate design points.
#'  * The batch evaluation is requested at the [TuningInstance] object `instance`,
#'    so each batch is possibly executed in parallel via [mlr3::benchmark()],
#'    and all evaluations are stored inside of `instance$bmr`.
#'  * Before and after the batch evaluation, the [Terminator] is checked, and if it is positive,
#'    an exception of class `"terminated_error"` is generated.
#'    In the later case the current batch of evaluations is still stored in `instance`,
#'    but the numeric scores are not sent back to the handling optimizer as it has lost execution control.
#'  * After such an exception was caught we select the best configuration from `instance$bmr` and
#'    return it.
#'  * Note that therefore more points than specified by the [Terminator] may be evaluated,
#'    as the Terminator is only checked before and after a batch evaluation,
#'    and not in-between evaluation in a batch.
#'    How many more depends on the setting of the batch size.
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
#' instance = TuningInstance$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measures = msr("classif.ce"),
#'   param_set = param_set,
#'   terminator = terminator
#' )
#' tt = TunerRandomSearch$new() # swap this line to use a different Tuner
#' res = tt$tune(instance) # returns best configuration and performance, and logs in 'instance'
#' instance$archive() # allows access of data.table / benchmark result of full path of all evaluations
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

    tune = function(instance) {
      not_supported_pclasses = setdiff(unique(instance$param_set$class), self$param_classes)
      if (length(not_supported_pclasses) > 0L)
        stopf("Tuner '%s' does not support param types: '%s'", class(self)[1L], paste0(not_supported_pclasses, collapse = ","))
      instance$start_time = Sys.time()
      lg$info("Starting to tune %i parameters with '%s' and '%s'" ,
        instance$param_set$length, self$format(), instance$terminator$format())
      lg$info("Terminator settings: %s", as_short_string(instance$terminator$settings))
      # run internal tune function which calls the optimizer
      # the optimizer will call eval_batch,
      # that will generate an exception when terminator is positive
      # we then catch that here and stop
      tryCatch({
        private$tune_internal(instance)
      }, terminated_error = function(cond) {
      })

      rr = instance$best(ties_method = self$ties_method)
      # FIXME: autotuner setting later
      lg$info("Finished tuning after %i evals", instance$n_evals)
      list(performance = rr$aggregate(instance$measures), values = rr$learners[[1L]]$param_set$values)
    }
  ),

  private = list(
    tune_internal = function(instance) {
      # every subclass has to implement this to call optimizer
      stop("abstract")
    }
  )
)
