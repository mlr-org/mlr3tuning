#' @title Tuner
#'
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
#' @section Private Methods:
#' * `.tune(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify tuning of your subclass.
#'   See technical details sections.
#' * `assign_result(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify how the final configuration is selected.
#'   See technical details sections.
#'
#' @section Technical Details and Subclasses:
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner
#'  * Specify the private abstract method `$.tune()` and use it to call into your optimizer.
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
#'  * Overwrite the private super-method `assign_result()` if you want to decide yourself how to estimate
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
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Set of control parameters for tuner.
    #'
    #' @param param_classes (`character()`)\cr
    #'   Supported parameter classes for learner hyperparameters that the tuner can optimize, subclasses of [paradox::Param].
    #'
    #' @param properties (`character()`)\cr
    #'   Set of properties of the tuner. Must be a subset of [`mlr_reflections$tuner_properties`][mlr3::mlr_reflections].
    #'
    #' @param packages (`character()`)\cr
    #'   Set of required packages.
    #'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
    initialize = function(param_set, param_classes, properties, packages = character()) {
      super$initialize(param_set = param_set, param_classes = param_classes,
                       properties = properties)
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Printer.
    #' @param ... (ignored).
    print = function() {
      catf(format(self))
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values)))
      catf(str_indent("* Packages:", self$packages))
      catf(str_indent("* Properties:", self$properties))
    },

    #' @description
    #' Performs the tuning on a [TuningInstance] until termination.
    #'
    #' @param instance (TuningInstance].
    #'
    #' @return Modifed `self`.
    optimize = function(inst) {
      assert_r6(inst, "TuningInstance")
      require_namespaces(self$packages)
      if ("dependencies" %nin% self$properties && inst$param_set$has_deps)
        stopf("Tuner '%s' does not support param sets with dependencies!", self$format())
      not_supported_pclasses = setdiff(unique(inst$param_set$class), self$param_classes)
      if (length(not_supported_pclasses) > 0L)
        stopf("Tuner '%s' does not support param types: '%s'", class(self)[1L], paste0(not_supported_pclasses, collapse = ","))

      tryCatch({
        private$.optimize(inst)
      }, terminated_error = function(cond) {})

      private$assign_result(inst)
      invisible(self)
    }
  ),

  private = list(
    assign_result = function(inst) {
      tuner_assign_result_default(inst)
    }
  )
)

tuner_assign_result_default = function(inst) {
  assert_r6(inst, "TuningInstance")
  # get best RR - best by mean perf value and extract perf values

  res = inst$archive$get_best()
  perf = as.matrix(res[,inst$objective$codomain$ids(),with=FALSE])[1,]
  # get the untrafoed config that matches this RR
  pv = as.list(as.matrix(res[,inst$objective$domain$ids(),with=FALSE])[1,])

  inst$assign_result(pv, perf)
  invisible(NULL)
}
