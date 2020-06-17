#' @title TunerDesignPoints
#'
#' @name mlr_tuners_design_points
#'
#' @description
#' Subclass for tuning w.r.t. fixed design points.
#'
#' We simply search over a set of points fully specified by the user. The points
#' in the design are evaluated in order as given.
#'
#' In order to support general termination criteria and parallelization, we
#' evaluate points in a batch-fashion of size `batch_size`. Larger batches mean
#' we can parallelize more, smaller batches imply a more fine-grained checking
#' of termination criteria.
#'
#' @templateVar id design_points
#' @template section_dictionary_tuners
#'
#' @section Parameters:
#' \describe{
#' \item{`batch_size`}{`integer(1)`\cr
#' Maximum number of configurations to try in a batch.}
#' \item{`design`}{[data.table::data.table]\cr
#' Design points to try in search, one per row.}
#' }
#'
#' @export
#' @examples
#' library(mlr3)
#' library(paradox)
#' library(data.table)
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
#' design = data.table(cp = c(0.1, 0.3))
#' tt = tnr("design_points", design = design)
#' # modifies the instance by reference
#' tt$optimize(instance)
#' # returns best configuration and best performance
#' instance$result
#' # allows access of data.table of full path of all evaluations
#' instance$archive
TunerDesignPoints = R6Class("TunerDesignPoints",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("batch_size", lower = 1L, tags = "required"),
        ParamUty$new("design", tags = "required", custom_check = function(x) {
          check_data_table(x, min.rows = 1, min.cols = 1, null.ok = TRUE)
        })
      ))
      ps$values = list(batch_size = 1L, design = NULL)
      super$initialize(
        param_set = ps,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),

  private = list(
    .optimize = function(instance) {
      pv = self$param_set$values
      if (is.null(pv$design))
        stopf("Please set design datatable!")
      d = Design$new(pv$design,
        param_set = instance$search_space, remove_dupl = FALSE) # does assert for us
      ch = chunk_vector(seq_row(d$data), chunk_size = pv$batch_size,
        shuffle = FALSE)
      for (inds in ch) {
        instance$eval_batch(d$data[inds, ])
      }
    }
  )
)

mlr_tuners$add("design_points", TunerDesignPoints)
