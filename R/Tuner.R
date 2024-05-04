#' @title Tuner
#'
#' @include mlr_tuners.R
#'
#' @description
#' The `Tuner` implements the optimization algorithm.
#'
#' @details
#' `Tuner` is an abstract base class that implements the base functionality each tuner must provide.
#'
#' @section Resources:
#' There are several sections about hyperparameter optimization in the [mlr3book](https://mlr3book.mlr-org.com).
#'
#' * An overview of all tuners can be found on our [website](https://mlr-org.com/tuners.html).
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
#' @family Tuner
#' @template field_id
#'
#' @template param_id
#' @template param_param_set
#' @template param_param_classes
#' @template param_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @export
Tuner = R6Class("Tuner",
  public = list(

    id = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id = "tuner",
      param_set,
      param_classes,
      properties,
      packages = character(),
      label = NA_character_,
      man = NA_character_
      ) {
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
      assert_tuning_instance(inst)
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

