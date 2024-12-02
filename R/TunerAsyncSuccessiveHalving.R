#' @export
TunerAsyncSuccessiveHalving = R6Class("TunerAsyncSuccessiveHalving",
  inherit = TunerAsync,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta                   = p_dbl(lower = 1.0001, default = 2),
        sampler               = p_uty(custom_check = crate({function(x) check_r6(x, "Sampler", null.ok = TRUE)})))

      param_set$values = list(eta = 2, sampler = NULL)

      super$initialize(
        id = "async_successive_halving",
        param_set = param_set,
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit", "async"),
        packages = "rush",
        label = "Asynchronous Successive Halving",
        man = "bbotk::mlr_optimizers_async_successive_halving"
      )
    },

    #' @description
    #' Starts the asynchronous optimization.
    #'
    #' @param inst ([OptimInstance]).
    #' @return [data.table::data.table].
    optimize = function(inst) {
      pars = self$param_set$values
      n = pars$n
      eta = pars$eta
      sampler = pars$sampler
      search_space = inst$search_space
      budget_id = search_space$ids(tags = "budget")
      archive = inst$archive
      direction = inst$archive$codomain$maximization_to_minimization

      # check budget
      if (length(budget_id) != 1) stopf("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))

      # required for calculation of hypervolume
      if (inst$archive$codomain$length > 1) require_namespaces("emoa")

      # sampler
      search_space_sampler = search_space$clone()$subset(setdiff(search_space$ids(), budget_id))
      private$.sampler = if (is.null(sampler)) {
        sampler = SamplerUnif$new(search_space_sampler)
      } else {
        assert_set_equal(sampler$param_set$ids(), search_space_sampler$ids())
      }

      # top_n
      private$.top_n = if (archive$codomain$length == 1) {
        function(data, cols_y, n, direction) {
          setorderv(data, cols = cols_y, order = direction)
          head(data, n)
        }
      } else {
        function(data, cols_y, n, direction) {
          points = t(as.matrix(data[, cols_y, with = FALSE]))
          ii = nds_selection(points, n, minimize = direction == 1)
          data[ii]
        }
      }

      # r_min is the budget of a single configuration in the first stage
      # r_max is the maximum budget of a single configuration in the last stage
      # the internal budget is rescaled to a minimum budget of 1
      # for this, the budget is divided by r_min
      # the budget is transformed to the original scale before passing it to the objective function
      r_max = search_space$upper[[budget_id]]
      private$.r_min = r_min = search_space$lower[[budget_id]]

      # maximum budget of a single configuration in the last stage (scaled)
      r = r_max / r_min

      # number of stages if each configuration in the first stage uses the minimum budget
      # and each configuration in the last stage uses no more than maximum budget
      private$.s_max = floor(log(r, eta))

      optimize_async_default(inst, self)
    }
  ),

  private = list(
    .r_min = NULL,
    .s_max = NULL,
    .top_n = NULL,
    .sampler = NULL,

    .optimize = function(inst) {
      archive = inst$archive
      r_min = private$.r_min
      s_max = private$.s_max
      eta = self$param_set$values$eta
      budget_id = inst$search_space$ids(tags = "budget")
      direction = inst$archive$codomain$maximization_to_minimization

      while (!inst$is_terminated) {

        # sample new point in stage 1
        xdt = private$.sampler$sample(1)$data
        xs = transpose_list(xdt)[[1]]
        asha_id = uuid::UUIDgenerate()
        xs = c(xs, list(asha_id = asha_id, stage = 1))
        xs[[budget_id]] = private$.r_min

        # evaluate
        get_private(inst)$.eval_point(xs)

        for (s in seq(s_max)) {
          data_stage = archive$finished_data[list(s), , on = "stage"]
          n_promotable = max(floor(nrow(data_stage) / eta), 1)

          lg$debug("%i promotable configurations in stage %i", n_promotable, s)

          candidates = private$.top_n(data_stage, archive$cols_y, n_promotable, direction)

          if (asha_id %nin% candidates$asha_id) {
            lg$debug("Configuration %s is not promotable to stage %i", asha_id, s + 1)
            lg$debug("Best ys: %s", candidates[[archive$cols_y]])
            break
          }

          lg$debug("Configuration %s is promotable to stage %i", asha_id, s + 1)

          # increase budget of xs
          rs = r_min * eta^s
          xs[[budget_id]] = rs
          xs$stage = s + 1

          # evaluate
          get_private(inst)$.eval_point(xs)
        }
      }
    }
  )
)

mlr_tuners$add("async_successive_halving", TunerAsyncSuccessiveHalving)

if (FALSE) {
  flush_redis()
  library(rush)

  options(bbotk_local = TRUE)
  lgr::get_logger("bbotk")$set_threshold("debug")

  learner = lrn("classif.debug",
    x = to_tune(0, 1),
    iter = to_tune(p_int(1, 16, tags = "budget")))

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE
  )

  tuner = tnr("async_successive_halving")
  tuner$optimize(instance)


  instance$archive

  ## hotstart
  flush_redis()
  library(rush)

  options(bbotk_local = TRUE)
  lgr::get_logger("bbotk")$set_threshold("debug")

  learner = lrn("classif.debug",
    x = to_tune(0, 1),
    iter = to_tune(p_int(1, 16, tags = "budget")))

  callback_async_hotstart = callback_async_tuning(
    id = "mlr3tuning.async_hotstart",

    on_eval_after_xs = function(callback, context) {
      context$instance$objective$store_models = TRUE

      if (context$extra$stage > 1) {
        context$instance$objective$learner$hotstart_stack = HotstartStack$new(learners = callback$state$learners)
      }
    },

    on_eval_after_resample = function(callback, context) {
      callback$state$learners = context$resample_result$learners
      context$instance$objective$learner$hotstart_stack = NULL
      context$resample_result$discard(models = TRUE)
    }
  )

  rush_plan(n_workers = 2)
  instance = ti_async(
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measure = msr("classif.ce"),
    terminator = trm("evals", n_evals = 20),
    store_benchmark_result = FALSE,
    callbacks = callback_async_hotstart,
    store_models = TRUE
  )

  tuner = tnr("async_successive_halving")
  tuner$optimize(instance)

  instance$archive
}



