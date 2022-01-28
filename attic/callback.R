devtools::load_all("../bbotk")
library(mlr3tuning)
library(mlr3tuningspaces)
library(mlr3learners)
library(R6)

future::plan("multisession", workers = 4)
#options(future.debug = TRUE)

CallbackEarlyStopping = R6Class("CallbackEarlyStopping", inherit = Callbacks,
  private = list(
    .on_eval_many_end = function(ydt, bmr, design, learners, objective) {
      mean_nrounds = round(mlr3misc::map_dbl(bmr$resample_results$resample_result, function(rr) {
        mean(mlr3misc::map_dbl(mlr3misc::get_private(rr)$.data$learner_states(mlr3misc::get_private(rr)$.view), function(state) state$model$niter))
      }))
      data.table::set(ydt, j = "mean_nrounds", value = mean_nrounds)
    },

    .on_eval_end = function(ys, rr, objective) {
      mean_nrounds = mean(mlr3misc::map_dbl(mlr3misc::get_private(rr)$.data$learner_states(), function(state) state$model$niter))
      c(ys, "mean_nrounds" = mean_nrounds)
    }
  )
)

# retrieve task
task = tsk("spam")

# load learner and set search space
learner = lts(lrn("classif.xgboost", predict_type = "prob", early_stopping_rounds = 20, early_stopping_test_set = TRUE))
learner$param_set$values$nrounds = 2000

# hyperparameter tuning on the pima indians diabetes data set
instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.auc"),
  terminator = trm("run_time", secs = 600),
  callbacks = CallbackEarlyStopping$new()
)

tuner = tnr("async_random_search")

tuner$optimize(instance)


