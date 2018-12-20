library(R6)
library(mlr3)
library(data.table)
devtools::load_all()

TunerNestedResampling = R6Class("TunerNestedResampling",
  inherit = Tuner,
  public = list(
    tuner = NULL,
    resampling_outer = NULL,
    bmr_outer = NULL,
    # param_best = NULL,
    # terminator = NULL, # in parent class

    initialize = function(tuner, resampling) {
      self$tuner = checkmate::assert_r6(tuner, "Tuner")
      self$resampling_outer = checkmate::assert_r6(resampling, "Resampling")
      self$resampling_outer$instantiate(tuner$ff$task)
      self$terminator = TerminatorIterations$new(resampling$iters)
      self$ff = tuner$ff$clone()
      self$ff$resampling$instantiate(self$ff$task)
      # self$ff$resampling = assert_r6(resampling, "Resampling")
    }
  ),

  private = list(
    tune_step = function() {
      tuner_temp = self$tuner$clone()
      # +1 because the terminator starts counting with 0
      train_set_temp = self$ff$resampling$train_set(self$terminator$state$evals + 1L)
      tuner_temp$ff$task$filter(train_set_temp)
      tuner_remp$tune()

      # Fill param_best with best parameter setting from previous tuning

      resampling_temp = ResamplignCustom$new()
      resampling_temp$instantiate(self$ff$task, train_set_temp)
      self$ff$resampling = resampling_temp
      self$ff$eval(tuner_temp$tune_result()$param_vals)
    }
  )
)

# task = mlr3::mlr_tasks$get("spam")
# learner = mlr3::mlr_learners$get("classif.rpart")
# learner$predict_type = "prob"
# resampling = mlr3::mlr_resamplings$get("holdout")
# measures = mlr3::mlr_measures$mget(c("auc", "mmce"))
# param_set = paradox::ParamSet$new(
#   params = list(
#     paradox::ParamDbl$new("cp", lower = 0.001, upper = 0.1),
#     paradox::ParamInt$new("minsplit", lower = 2, upper = 5)
#   ),
# )

# ff = FitnessFunction$new(
#   task = task,
#   learner = learner,
#   resampling = resampling,
#   measures = measures,
#   param_set = param_set,
#   ctrl = tune_control(store_prediction = TRUE) # for the exceptions
# )

# terminator = TerminatorEvaluations$new(5)
# gs = TunerRandomSearch$new(ff, terminator)
# # gs$tune()

# outer = mlr_resamplings$get("cv")
# nested = TunerNestedResampling$new(gs, outer)
# nested$tune()