#' @title Tuner
#'
#' @include mlr_tuners.R
#'
#' @description
#' Abstract `Tuner` class that implements the base functionality each tuner must
#' provide. A tuner is an object that describes the tuning strategy, i.e. how to
#' optimize the black-box function and its feasible set defined by the
#' [TuningInstance] object.
#'
#' A list of measures can be passed to the instance, and they will always be all
#' evaluated. However, single-criteria tuners optimize only the first measure.
#'
#' A tuner must write its result into the [TuningInstance] using the
#' `assign_result` method of the [bbotk::OptimInstance] at the end of its tuning
#' in order to store the best selected hyperparameter configuration and its
#' estimated performance vector.
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
#'  * Inherit from Tuner
#'  * Specify the private abstract method `$.tune()` and use it to call into
#'  your optimizer.
#'  * You need to call `instance$eval_batch()` to evaluate design points.
#'  * The batch evaluation is requested at the [TuningInstance] object
#'  `instance`, so each batch is possibly executed in parallel via
#'  [mlr3::benchmark()], and all evaluations are stored inside of
#'  `instance$archive`.
#'  * Before the batch evaluation, the [Terminator] is checked, and if it is
#'  positive, an exception of class `"terminated_error"` is generated. In the
#'  later case the current batch of evaluations is still stored in `instance`,
#'  but the numeric scores are not sent back to the handling optimizer as it has
#'  lost execution control.
#'  * After such an exception was caught we select the best configuration from
#'  `instance$archive` and return it.
#'  * Note that therefore more points than specified by the [Terminator] may be
#'  evaluated, as the Terminator is only checked before a batch evaluation, and
#'  not in-between evaluation in a batch. How many more depends on the setting
#'  of the batch size.
#'  * Overwrite the private super-method `.assign_result()` if you want to decide
#'  yourself how to estimate the final configuration in the instance and its
#'  estimated performance. The default behavior is: We pick the best
#'  resample-experiment, regarding the given measure, then assign its
#'  configuration and aggregated performance to the instance.
#'
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' search_space = ParamSet$new(list(
#'   ParamDbl$new("cp", lower = 0.001, upper = 0.1)
#' ))
#' terminator = term("evals", n_evals = 3)
#' instance = TuningInstance$new(
#'   task = tsk("iris"),
#'   learner = lrn("classif.rpart"),
#'   resampling = rsmp("holdout"),
#'   measure = msr("classif.ce"),
#'   search_space = search_space,
#'   terminator = terminator
#' )
#' # swap this line to use a different Tuner
#' tt = tnr("random_search")
#' # modifies the instance by reference
#' tt$optimize(instance)
#' # returns best configuration and best performance
#' instance$result
#' # allows access of data.table / benchmark result of full path of all
#' # evaluations
#' instance$archive
Tuner = R6Class("Tuner",
  public = list(
    #' @field param_set ([paradox::ParamSet]).
    param_set = NULL,

    #' @field param_classes (`character()`).
    param_classes = NULL,

    #' @field properties (`character()`).
    properties = NULL,

    #' @field packages (`character()`).
    packages = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param param_set [paradox::ParamSet]\cr
    #' Set of control parameters for tuner.
    #'
    #' @param param_classes `character()`\cr
    #' Supported parameter classes for learner hyperparameters that the tuner
    #' can optimize, subclasses of [paradox::Param].
    #'
    #' @param properties `character()`\cr
    #' Set of properties of the tuner. Must be a subset of
    #' [`mlr_reflections$tuner_properties`][mlr3::mlr_reflections].
    #'
    #' @param packages `character()`\cr
    #' Set of required packages. Note that these packages will be loaded via
    #' [requireNamespace()], and are not attached.
    initialize = function(param_set, param_classes, properties,
      packages = character()) {
      self$param_set = assert_param_set(param_set)
      self$param_classes = assert_subset(
        param_classes,
        c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct", "ParamUty"))
      # has to have at least multi-crit or single-crit property
      self$properties = assert_subset(properties,
        bbotk_reflections$optimizer_properties,
        empty.ok = FALSE)
      self$packages = assert_set(packages)
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
    #' Performs the tuning on a [TuningInstance] until termination.
    #'
    #' @param inst [TuningInstance].
    #'
    #' @return Modified `self`.
    optimize = function(inst) {
      # TuningInstanceMulticrit actually does not inherit from TuningInstance
      # but from OptimInstanceMulticrit in the same way as TuningInstance
      # inherits from OptimInstance. Unfortunately multi-inheritance is not in
      # R6.

      assert_multi_class(inst, c("TuningInstance", "TuningInstanceMulticrit"))
      assert_instance_properties(self, inst)

      tryCatch({
        private$.optimize(inst)
      }, terminated_error = function(cond) {
      })
      private$.assign_result(inst)
      invisible(NULL)
    }
  ),

  private = list(
    .optimize = function(inst) stop("abstract"),

    .assign_result = function(inst) {
      assert_r6(inst, "TuningInstance")
      res = inst$archive$best()

      xdt = res[, inst$search_space$ids(), with = FALSE]

      if (inherits(inst, "TuningInstanceMulticrit")) {
        ydt = res[, inst$objective$codomain$ids(), with = FALSE]
        inst$assign_result(xdt, ydt)
      } else {
        # unlist keeps name!
        y = unlist(res[, inst$objective$codomain$ids(), with = FALSE])
        inst$assign_result(xdt, y)
      }

      invisible(NULL)
    }
  )
)
