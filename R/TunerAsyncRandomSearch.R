#' @title Hyperparameter Tuning with Random Search
#'
#' @name mlr_tuners_async_random_search
#' @include Tuner.R
#'
#' @description
#' Subclass for random search tuning.
#'
#' @details
#' The random points are sampled by [paradox::generate_design_random()].
#'
#' @templateVar id random_search
#' @template section_dictionary_tuners
#'
#' @inheritSection bbotk::OptimizerRandomSearch Parameters
#' @inheritSection Tuner Resources
#' @inheritSection bbotk::OptimizerRandomSearch Progress Bars
#' @template section_parallelization
#' @template section_logging
#' @templateVar optimizer bbotk::OptimizerRandomSearch
#' @template section_optimizer
#'
#' @source
#' `r format_bib("bergstra_2012")`
#'
#' @family Tuner
#' @seealso Package \CRANpkg{mlr3hyperband} for hyperband tuning.
#' @export
#' @template example
TunerAsyncRandomSearch = R6Class("TunerAsyncRandomSearch",
  inherit = Tuner,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "async_random_search",
        param_set = ps(),
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit", "async"),
        label = "Async Random Search",
        man = "bbotk::mlr_optimizers_async_random_search"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      sampler = SamplerUnif$new(inst$search_space)
      n = inst$archive$free_workers * 10
      repeat { # iterate until we have an exception from eval_async
        if (inst$archive$length_queue_eval < n) {
          design = sampler$sample(n)
          inst$eval_async(design$data)
        }
        Sys.sleep(0.05)
      }
    }
  )
)

mlr_tuners$add("async_random_search", TunerAsyncRandomSearch)
