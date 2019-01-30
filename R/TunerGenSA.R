TunerGenSA = R6Class("TunerGenSA",
  inherit = Tuner,
  public = list(
    initialize = function(ff, terminator, ...) {
      if (any(ff$param_set$storage_type != "numeric")) {
        logger::log_error("Parameter types needs to be numeric", namespace = "mlr3")
      }
      super$initialize(id = "GenSA", ff = ff, terminator = terminator, settings = list(...))
    }
  ),
  private = list(
    tune_step = function() {
      objective = function (x, ff) {
        # Generate design from new proposed parameter value:
        param_value = mlr3misc::set_names(list(x), nm = ff$param_set$ids())
        dt_param_value = do.call(data.table::data.table, param_value)

        private$eval_design_terminator(paradox::Design$new(ff$param_set, dt_param_value))

        performance = unlist(ff$bmr$data[.N]$performance)[[1]]
        if (! ff$task$measures[[1]]$minimize)
          return(-performance)
        return(performance)
      }
      nuisance = GenSA(fn = objective, lower = self$ff$param_set$lower, upper = self$ff$param_set$upper,
        control = self$settings, ff = self$ff)
    }
  )
)

