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
#' @inheritSection TuningInstanceBatchSingleCrit Resources
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
  inherit = Mlr3Component,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(
      id,
      param_set,
      param_classes,
      properties,
      packages = character(0),
      label,
      man
    ) {
      if (!missing(label) || !missing(man)) {
        deprecated_component("label and man are deprecated for Tuner construction and will be removed in the future.")
      }

      super$initialize(dict_entry = id, dict_shortaccess = "tnr",
        param_set = param_set, packages = packages, properties = properties
      )
      private$.param_classes = assert_subset(param_classes, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct", "ParamUty"))
      # has to have at least multi-crit or single-crit property
      assert_subset(properties, bbotk_reflections$optimizer_properties, empty.ok = FALSE)
    },

    #' @description
    #' Print method.
    #'
    #' @return (`character()`).
    print = function() {
      msg_h = if (is.na(self$label)) "" else paste0(": ", self$label)
      msg_params = cli_vec(map_chr(self$param_classes, function(p) format_inline('{.cls {p}}')),
                           style = list(last = ' and ', sep = ', '))
      cat_cli({
        cli_h1("{.cls {class(self)[1]}}{msg_h}")
        cli_li("Parameters: {as_short_string(self$param_set$values)}")
        cli_li("Parameter classes: {msg_params}")
        cli_li("Properties: {self$properties}")
        cli_li("Packages: {.pkg {self$packages}}")
      })
    },

    #' @description
    #' Opens the corresponding help page referenced by field `$man`.
    help = function() {
      open_help(self$man)
    }
  ),

  active = list(
    #' @field param_classes (`character()`)\cr
    #' Supported parameter classes for learner hyperparameters that the tuner can optimize, as given in the [paradox::ParamSet] `$class` field.
    param_classes = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.param_classes)) {
        stop("$param_classes is read-only.")
      }
      private$.param_classes
    }
  ),

  private = list(
    .optimize = function(inst) stop("abstract"),

    .assign_result = function(inst) {
      assert_tuning_instance(inst)
      assign_result_default(inst)
    },

    .param_classes = NULL
  )
)

