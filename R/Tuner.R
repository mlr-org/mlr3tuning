#' @title Class for Tuning Algorithms
#'
#' @include mlr_tuners.R
#'
#' @description
#' The [Tuner] implements the optimization algorithm.
#'
#' @details
#' [Tuner] is a abstract base class that implements the base functionality each tuner must provide.
#' A subclass is implemented in the following way:
#'  * Inherit from Tuner.
#'  * Specify the private abstract method `$.optimize()` and use it to call into your optimizer.
#'  * You need to call `instance$eval_batch()` to evaluate design points.
#'  * The batch evaluation is requested at the [TuningInstanceSingleCrit]/[TuningInstanceMultiCrit] object `instance`, so each batch is possibly executed in parallel via [mlr3::benchmark()], and all evaluations are stored inside of `instance$archive`.
#'  * Before the batch evaluation, the [bbotk::Terminator] is checked, and if it is positive, an exception of class `"terminated_error"` is generated.
#'    In the  later case the current batch of evaluations is still stored in `instance`, but the numeric scores are not sent back to the handling optimizer as it has lost execution control.
#'  * After such an exception was caught we select the best configuration from `instance$archive` and return it.
#'  * Note that therefore more points than specified by the [bbotk::Terminator] may be evaluated, as the Terminator is only checked before a batch evaluation, and not in-between evaluation in a batch.
#'    How many more depends on the setting of the batch size.
#'  * Overwrite the private super-method `.assign_result()` if you want to decide yourself how to estimate the final configuration in the instance and its estimated performance.
#'    The default behavior is: We pick the best resample-experiment, regarding the given measure, then assign its configuration and aggregated performance to the instance.
#'
#' @section Private Methods:
#' * `.optimize(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify tuning of your subclass.
#'   See details sections.
#' * `.assign_result(instance)` -> `NULL`\cr
#'   Abstract base method. Implement to specify how the final configuration is selected.
#'   See details sections.
#'
#' @section Resources:
#' There are several sections about hyperparameter optimization in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#'  * Learn more about [tuners](https://mlr3book.mlr-org.com/chapters/chapter4/hyperparameter_optimization.html#sec-tuner).
#'
#' The [gallery](https://mlr-org.com/gallery-all-optimization.html) features a collection of case studies and demos about optimization.
#'
#'  * Use the [Hyperband](https://mlr-org.com/gallery/series/2023-01-15-hyperband-xgboost/) optimizer with different budget parameters.
#'
#' @section Extension Packages:
#' Additional tuners are provided by the following packages.
#'
#'  * [mlr3hyperband](https://github.com/mlr-org/mlr3hyperband) adds the Hyperband and Successive Halving algorithm.
#'  * [mlr3mbo](https://github.com/mlr-org/mlr3mbo) adds Bayesian optimization methods.
#'
#' @template param_man
#'
#' @export
Tuner = R6Class("Tuner",
  public = list(

    #' @field id (`character(1)`)\cr
    #'   Identifier of the object.
    #'   Used in tables, plot and text output.
    id = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier for the new instance.
    #'
    #' @param param_set ([paradox::ParamSet])\cr
    #'   Set of control parameters.
    #'
    #' @param param_classes (`character()`)\cr
    #'   Supported parameter classes for learner hyperparameters that the tuner can optimize, as given in the [paradox::ParamSet] `$class` field.
    #'
    #' @param properties (`character()`)\cr
    #'   Set of properties of the tuner.
    #'   Must be a subset of [`mlr_reflections$tuner_properties`][mlr3::mlr_reflections].
    #'
    #' @param packages (`character()`)\cr
    #'   Set of required packages.
    #'   Note that these packages will be loaded via [requireNamespace()], and are not attached.
    #'
    #' @param label (`character(1)`)\cr
    #'   Label for this object.
    #'   Can be used in tables, plot and text output instead of the ID.
    initialize = function(id = "tuner", param_set, param_classes, properties, packages = character(), label = NA_character_, man = NA_character_) {
      self$id = assert_string(id, min.chars = 1L)
      private$.param_set = assert_param_set(param_set)
      private$.param_classes = assert_subset(param_classes, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct", "ParamUty"))
      # has to have at least multi-crit or single-crit property
      private$.properties = assert_subset(properties, bbotk_reflections$optimizer_properties, empty.ok = FALSE)
      private$.packages = union("mlr3tuning", assert_character(packages, any.missing = FALSE, min.chars = 1L))
      private$.label = assert_string(label, na.ok = TRUE)
      private$.man = assert_string(man, na.ok = TRUE)

      check_packages_installed(self$packages, msg = sprintf("Package '%%s' required but not installed for Tuner '%s'", format(self)))
    },

    #' @description
    #' Helper for print outputs.
    #'
    #' @return (`character()`).
    #' @param ... (ignored).
    format = function(...) {
      sprintf("<%s>", class(self)[1L])
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      catn(format(self), if (is.na(self$label)) "" else paste0(": ", self$label))
      catn(str_indent("* Parameters:", as_short_string(self$param_set$values)))
      catn(str_indent("* Parameter classes:", self$param_classes))
      catn(str_indent("* Properties:", self$properties))
      catn(str_indent("* Packages:", self$packages))
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    },

    #' @description
    #' Performs the tuning on a [TuningInstanceSingleCrit] or [TuningInstanceMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveTuning] that resides in the [TuningInstanceSingleCrit]/[TuningInstanceMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([TuningInstanceSingleCrit] | [TuningInstanceMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      assert_multi_class(inst, c("TuningInstanceSingleCrit", "TuningInstanceMultiCrit"))
      inst$archive$start_time = Sys.time()
      inst$.__enclos_env__$private$.context = ContextOptimization$new(instance = inst, optimizer = self)
      call_back("on_optimization_begin", inst$callbacks, get_private(inst)$.context)

      # evaluate learner with default hyperparameter values
      if (get_private(inst)$.evaluate_default) evaluate_default(inst)

      result = optimize_default(inst, self, private)
      call_back("on_optimization_end", inst$callbacks, get_private(inst)$.context)
      if (!inst$objective$keep_hotstart_stack) inst$objective$hotstart_stack = NULL
      result
    }
  ),

  active = list(

    #' @field param_set ([paradox::ParamSet])\cr
    #' Set of control parameters.
    param_set = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("$param_set is read-only.")
      }
      private$.param_set
    },

    #' @field param_classes (`character()`)\cr
    #' Supported parameter classes for learner hyperparameters that the tuner can optimize, as given in the [paradox::ParamSet] `$class` field.
    param_classes = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_classes)) {
        stop("$param_classes is read-only.")
      }
      private$.param_classes
    },

    #' @field properties (`character()`)\cr
    #' Set of properties of the tuner.
    #' Must be a subset of [`mlr_reflections$tuner_properties`][mlr3::mlr_reflections].
    properties = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.properties)) {
        stop("$properties is read-only.")
      }
      private$.properties
    },

    #' @field packages (`character()`)\cr
    #' Set of required packages.
    #' Note that these packages will be loaded via [requireNamespace()], and are not attached.
    packages = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.packages)) {
        stop("$packages is read-only.")
      }
      private$.packages
    },

    #' @field label (`character(1)`)\cr
    #' Label for this object.
    #' Can be used in tables, plot and text output instead of the ID.
    label = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_set)) {
        stop("$label is read-only.")
      }
      private$.label
    },

    #' @field man (`character(1)`)\cr
    #' String in the format `[pkg]::[topic]` pointing to a manual page for this object.
    #' The referenced help package can be opened via method `$help()`.
    man = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.man)) {
        stop("$man is read-only.")
      }
      private$.man
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
    .packages = NULL,
    .label = NULL,
    .man = NULL
  )
)

