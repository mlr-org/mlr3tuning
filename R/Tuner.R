#' @title Tuner
#'
#' @include mlr_tuners.R
#'
#' @description
#' Abstract `Tuner` class that implements the base functionality each tuner must
#' provide. A tuner is an object that describes the tuning strategy, i.e. how to
#' optimize the black-box function and its feasible set defined by the
#' [TuningInstanceSingleCrit] / [TuningInstanceMultiCrit] object.
#'
#' A tuner must write its result into the [TuningInstanceSingleCrit] /
#' [TuningInstanceMultiCrit] using the `assign_result` method of the
#' [bbotk::OptimInstance] at the end of its tuning in order to store the best
#' selected hyperparameter configuration and its estimated performance vector.
#'
#' @section Private Methods:
#' * `.optimize(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify tuning of your subclass.
#'   See technical details sections.
#' * `.assign_result(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify how the final configuration is
#'   selected. See technical details sections.
#'
#' @section Technical Details and Subclasses:
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner.
#'  * Specify the private abstract method `$.tune()` and use it to call into
#'  your optimizer.
#'  * You need to call `instance$eval_batch()` to evaluate design points.
#'  * The batch evaluation is requested at the [TuningInstanceSingleCrit] /
#'  [TuningInstanceMultiCrit] object `instance`, so each batch is possibly
#'  executed in parallel via [mlr3::benchmark()], and all evaluations are stored
#'  inside of `instance$archive`.
#'  * Before the batch evaluation, the [bbotk::Terminator] is checked, and if it is
#'  positive, an exception of class `"terminated_error"` is generated. In the
#'  later case the current batch of evaluations is still stored in `instance`,
#'  but the numeric scores are not sent back to the handling optimizer as it has
#'  lost execution control.
#'  * After such an exception was caught we select the best configuration from
#'  `instance$archive` and return it.
#'  * Note that therefore more points than specified by the [bbotk::Terminator]
#'  may be evaluated, as the Terminator is only checked before a batch
#'  evaluation, and not in-between evaluation in a batch. How many more depends
#'  on the setting of the batch size.
#'  * Overwrite the private super-method `.assign_result()` if you want to decide
#'  yourself how to estimate the final configuration in the instance and its
#'  estimated performance. The default behavior is: We pick the best
#'  resample-experiment, regarding the given measure, then assign its
#'  configuration and aggregated performance to the instance.
#'
#' @export
#' @examples
#' instance = TuningInstanceSingleCrit$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   terminator = trm("evals", n_evals = 3)
#' )
#' tuner = tnr("random_search")
#'
#' # optimize hyperparameter
#' # modifies the instance by reference
#' tuner$optimize(instance)
#'
#' # returns best configuration and best performance
#' instance$result
#'
#' # allows access of data.table of full path of all evaluations
#' instance$archive
Tuner = R6Class("Tuner",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param param_set ([paradox::ParamSet])\cr
    #' Set of control parameters for tuner.
    #'
    #' @param param_classes (`character()`)\cr
    #' Supported parameter classes for learner hyperparameters that the tuner
    #' can optimize, subclasses of [paradox::Param].
    #'
    #' @param properties (`character()`)\cr
    #' Set of properties of the tuner. Must be a subset of
    #' [`mlr_reflections$tuner_properties`][mlr3::mlr_reflections].
    #'
    #' @param packages (`character()`)\cr
    #' Set of required packages. Note that these packages will be loaded via
    #' [requireNamespace()], and are not attached.
    initialize = function(param_set, param_classes, properties, packages = character()) {
      private$.param_set = assert_param_set(param_set)
      private$.param_classes = assert_subset(param_classes,
        c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct", "ParamUty"))
      # has to have at least multi-crit or single-crit property
      private$.properties = assert_subset(properties, bbotk_reflections$optimizer_properties, empty.ok = FALSE)
      private$.packages = union("mlr3tuning", assert_character(packages, any.missing = FALSE, min.chars = 1L))

      check_packages_installed(self$packages,
        msg = sprintf("Package '%%s' required but not installed for Tuner '%s'", format(self)))
    },

    #' @description
    #' Helper for print outputs.
    format = function() {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #' @return (`character()`).
    print = function() {
      catf(format(self))
      catf(str_indent("* Parameters:", as_short_string(self$param_set$values)))
      catf(str_indent("* Parameter classes:", self$param_classes))
      catf(str_indent("* Properties:", self$properties))
      catf(str_indent("* Packages:", self$packages))
    },

    #' @description
    #' Performs the tuning on a [TuningInstanceSingleCrit] or
    #' [TuningInstanceMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveTuning] that resides in the
    #' [TuningInstanceSingleCrit]/[TuningInstanceMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([TuningInstanceSingleCrit] | [TuningInstanceMultiCrit]).
    #'
    #' @return [data.table::data.table]
    optimize = function(inst) {
      assert_multi_class(inst, c("TuningInstanceSingleCrit", "TuningInstanceMultiCrit"))
      res = optimize_default(inst, self, private)
      if (!inst$objective$keep_hotstart_stack) inst$objective$hotstart_stack = NULL
      res
    }
  ),

  active = list(

    #' @field param_set ([paradox::ParamSet]).
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("$param_set is read-only.")
      }
      private$.param_set
    },

    #' @field param_classes (`character()`).
    param_classes = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_classes)) {
        stop("$param_classes is read-only.")
      }
      private$.param_classes
    },

    #' @field properties (`character()`).
    properties = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.properties)) {
        stop("$properties is read-only.")
      }
      private$.properties
    },

    #' @field packages (`character()`).
    packages = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.packages)) {
        stop("$packages is read-only.")
      }
      private$.packages
    }
  ),

  private = list(
    .optimize = function(inst) stop("abstract"),

    .assign_result = function(inst) {
      assert_multi_class(inst, c("TuningInstanceSingleCrit", "TuningInstanceMultiCrit"))
      assign_result_default(inst)
    },

    .param_set = NULL,
    .param_classes = NULL,
    .properties = NULL,
    .packages = NULL
  )
)
