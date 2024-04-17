#' @title Class for Tuning Algorithms
#'
#' @include mlr_tuners.R
#'
#' @description
#' The [TunerAsync] implements the asynchronous optimization algorithm.
#'
#' @template param_man
#'
#' @export
TunerAsync = R6Class("TunerAsync",
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
    #'   Supported parameter classes for learner hyperparameters that the tuner can optimize.
    #'   Subclasses of [paradox::Param].
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
    #' Performs the tuning on a [TuningInstanceAsyncSingleCrit] or [TuningInstanceAsyncMultiCrit] until termination.
    #' The single evaluations will be written into the [ArchiveAsyncTuning] that resides in the [TuningInstanceAsyncSingleCrit]/[TuningInstanceAsyncMultiCrit].
    #' The result will be written into the instance object.
    #'
    #' @param inst ([TuningInstanceAsyncSingleCrit]| [TuningInstanceAsyncMultiCrit]).
    #'
    #' @return [data.table::data.table()]
    optimize = function(inst) {
      stop("abstract")
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
    #' Supported parameter classes for learner hyperparameters that the tuner can optimize.
    #' Subclasses of [paradox::Param].
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
    # runs asynchronously on the workers
    .optimize = function(inst) {
      stop("abstract")
    },

    .assign_result = function(inst) {
      assert_multi_class(inst, c("TuningInstanceAsyncSingleCrit", "TuningInstanceAsyncMultiCrit"))
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

