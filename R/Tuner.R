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
#' If the tuning instance contains multiple measures, they will always be all evaluated.
#' But single-criteria tuners always optimize the first measure in the passed list.
#'
#' @section Construction:
#' ```
#' tuner = Tuner$new(param_set = ParamSet$new(), param_classes = character(),
#'   properties = character(), packages = character())
#' ```
#'
#' * `param_set` :: [paradox::ParamSet]\cr
#'   Set of control parameters for tuner.
#'
#' * `param_classes` :: `character()`\cr
#'   Supported parameter classes for learner hyperparameters that the tuner can optimize, subclasses of [paradox::Param].
#'
#' * `properties` :: `character()`\cr
#'   Set of properties of the tuner. Must be a subset of [`mlr_reflections$tuner_properties`][mlr_reflections].
#'
#' * `packages` :: `character()`\cr
#'   Set of required packages.
#'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
#'
#' @section Fields:
#' * `param_set` :: [paradox::ParamSet]\cr
#' * `param_classes` :: `character()`\cr
#' * `properties` :: `character()`\cr
#' * `packages` :: `character()`\cr
#'
#' @section Methods:
#' * `tune(instance)`\cr
#'   [TuningInstance] -> `self`\cr
#'   Performs the tuning on a [TuningInstance] until termination.
#'
#' * `tune_result(instance)`\cr
#'   [TuningInstance] -> named `list()`\cr
#'   Returns the "best" tuning result as named list:
#'     - `"performance"` (`numeric()`) with the best performance.
#'     - `"values"` (named `list()`) with the corresponding hyperparameters values.
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
#' terminator = term("evals", n_evals = 3)
#' instance = TuningInstance$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measures = msr("classif.ce"),
#'   param_set = param_set,
#'   terminator = terminator
#' )
#' tt = tnr("random_search") # swap this line to use a different Tuner
#' tt$tune(instance) # modifies the instance by reference
#' tt$tune_result(instance) # returns best configuration and performance
#' instance$archive() # allows access of data.table / benchmark result of full path of all evaluations
Tuner = R6Class("Tuner",
  public = list(
    param_set = NULL,
    param_classes = NULL,
    properties = NULL,
    packages = NULL,

    initialize = function(param_set = ParamSet$new(), param_classes = character(), properties = character(), packages = character()) {
      self$param_set = assert_param_set(param_set)
      self$param_classes = param_classes
      self$properties = assert_subset(properties, mlr_reflections$tuner_properties)
      self$packages = assert_set(packages)
    },

    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    print = function() {
      catf(format(self))
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values)))
      catf(str_indent("* Packages:", self$packages))
      catf(str_indent("* Properties:", self$properties))
    },

    tune = function(instance) {
      require_namespaces(self$packages)
      if ("dependencies" %nin% self$properties && instance$param_set$has_deps)
        stopf("Tuner '%s' does not support param sets with dependencies!", self$format())
      not_supported_pclasses = setdiff(unique(instance$param_set$class), self$param_classes)
      if (length(not_supported_pclasses) > 0L)
        stopf("Tuner '%s' does not support param types: '%s'", class(self)[1L], paste0(not_supported_pclasses, collapse = ","))
      instance$start_time = Sys.time()
      lg$info("Starting to tune %i parameters with '%s' and '%s'" ,
        instance$param_set$length, self$format(), instance$terminator$format())
      lg$info("Terminator settings: %s", as_short_string(instance$terminator$param_set$values))
      # run internal tune function which calls the optimizer
      # the optimizer will call eval_batch,
      # that will generate an exception when terminator is positive
      # we then catch that here and stop
      tryCatch({
        private$tune_internal(instance)
      }, terminated_error = function(cond) { })

      lg$info("Finished tuning after %i evals", instance$n_evals)
      invisible(self)
    },

    tune_result = function(instance) {
      rr = instance$best()
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
