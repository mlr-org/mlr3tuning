TunerRandomSearchAsync = R6Class("TunerRandomSearchAsync", inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        param_set = ps(),
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      inst$async = TRUE

      repeat({
        while (inst$archive$n_in_progress < 8) {
          print(inst$archive$data)
          xdt = generate_design_random(inst$search_space, 1)$data
          inst$archive$add_evals(xdt, status = "proposed")
          inst$eval_proposed(async = TRUE, single_worker = FALSE)
        }
      inst$archive$resolve_promise()
      })
    }
  )
)
