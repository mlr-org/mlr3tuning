#' @title Tuner
#'
#' @usage NULL
#' @format [R6::R6Class] object.
#' @include mlr_tuners.R
#'
#' @description
#' Abstract `Tuner` class that implements the base functionality each tuner must provide.
#' A tuner is an object that describes the tuning strategy, i.e. how to optimize the black-box
#' function and its feasible set defined by the [TuningInstance] object.
#'
#' A list of measures can be passed to the instance, and they will always be all evaluated.
#' However, single-criteria tuners optimize only the first measure.
#'
#' A tuner must write its result to the `assign_result` method of the [Tuninginstance] at the end of its tuning in
#' order to store the best selected hyperparameter configuration and its estimated performance vector.
#'
#' @section Construction:
#' ```
#' tuner = Tuner$new(param_set, param_classes, properties, packages = character())
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
#' * `param_set` :: [paradox::ParamSet]; from construction.
#' * `param_classes` :: `character()`\cr
#' * `properties` :: `character(); from construction.
#' * `packages` :: `character()`; from construction.
#'
#' @section Methods:
#' * `tune(instance)`\cr
#'   [TuningInstance] -> `self`\cr
#'   Performs the tuning on a [TuningInstance] until termination.
#'
#' @section Private Methods:
#' * `tune_internal(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify tuning of your subclass.
#'   See technical details sections.
#' * `assign_result(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify how the final configuration is selected.
#'   See technical details sections.
#'
#' @section Technical Details and Subclasses:
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner
#'  * Specify the private abstract method `$tune_internal()` and use it to call into your optimizer.
#'  * You need to call `instance$eval_batch()` to evaluate design points.
#'  * The batch evaluation is requested at the [TuningInstance] object `instance`,
#'    so each batch is possibly executed in parallel via [mlr3::benchmark()],
#'    and all evaluations are stored inside of `instance$bmr`.
#'  * Before the batch evaluation, the [Terminator] is checked, and if it is positive,
#'    an exception of class `"terminated_error"` is generated.
#'    In the later case the current batch of evaluations is still stored in `instance`,
#'    but the numeric scores are not sent back to the handling optimizer as it has lost execution control.
#'  * After such an exception was caught we select the best configuration from `instance$bmr` and
#'    return it.
#'  * Note that therefore more points than specified by the [Terminator] may be evaluated,
#'    as the Terminator is only checked before a batch evaluation,
#'    and not in-between evaluation in a batch.
#'    How many more depends on the setting of the batch size.
#'  * Overwrite the private super-method `assign_result` if you want to decide yourself how to estimate
#'    the final configuration in the instance and its estimated performance.
#'    The default behavior is: We pick the best resample-experiment, regarding the first measure,
#'    then assign its configuration and aggregated performance to the instance.
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
#' instance$result # returns best configuration and best performance
#' instance$archive() # allows access of data.table / benchmark result of full path of all evaluations
Tuner = R6Class("Tuner",
  public = list(
    param_set = NULL,
    param_classes = NULL,
    properties = NULL,
    packages = NULL,

    initialize = function(param_set, param_classes, properties, packages = character()) {
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
      assert_r6(instance, "TuningInstance")
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
      }, terminated_error = function(cond) {})
      lg$info("Finished tuning after %i evals", instance$n_evals)
      private$assign_result(instance)
      lg$info("Tuned x: %s", as_short_string(instance$result$tune_x))
      lg$info("Tuned y: %s", as_short_string(as.list(instance$result$perf)))
      invisible(NULL)
    }
  ),

  private = list(
    tune_internal = function(instance) {
      # every subclass has to implement this to call optimizer
      stop("abstract")
    },

    # the default super-method to assign results for a Tuner at the end
    # - pick the best resample-experiment, regarding the first measure
    # - then assign its config and aggregated perf to instance
    # --> a Tuner can overwrite this if it wants to do something more fancy
    assign_result = function(instance) {
      tuner_assign_result_default(instance)
    }
  )
)

tuner_assign_result_default = function(instance) {
  assert_r6(instance, "TuningInstance")
  # get best RR - best by mean perf value and extract perf values
  rr = instance$best()
  perf = rr$aggregate(instance$measures)
  # get the untrafoed config that matches this RR
  pv = instance$bmr$rr_data[rr$uhash, on = "uhash"]$tune_x[[1L]]
  instance$assign_result(pv, perf)
  invisible(NULL)
}

