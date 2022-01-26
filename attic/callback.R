devtools::load_all(".")
devtools::load_all("../mlr3tuningspaces")

CallbackEarlyStopping = R6Class("CallbackEarlyStopping", inherit = Callbacks,
  private = list(
    .on_eval_many_end = function(ydt, bmr, design, learners, objective) {
      mean_nrounds = round(map_dbl(bmr$resample_results$resample_result, function(rr) {
        mean(map_dbl(get_private(rr)$.data$learner_states(get_private(rr)$.view), function(state) state$model$niter))
      }))
      set(ydt, j = "mean_nrounds", value = mean_nrounds)
    },

    .on_eval_end = function(ys, rr, objective) {
      mean_nrounds = mean(map_dbl(get_private(rr)$.data$learner_states(), function(state) state$model$niter))
      c(ys, "mean_nrounds" = mean_nrounds)
    }
  )
)

# retrieve task
task = tsk("pima")

# load learner and set search space
learner = lts(lrn("classif.xgboost", predict_type = "prob", early_stopping_rounds = 20, early_stopping_test_set = TRUE))
learner$param_set$values$nrounds = 2000

# hyperparameter tuning on the pima indians diabetes data set
instance = TuningInstanceSingleCrit$new(
  task = task,
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.auc"),
  terminator = trm("evals", n_evals = 10),
  callbacks = CallbackEarlyStopping$new()
)

tuner = tnr("async_random_search")
tuner = tnr("random_search")

tuner$optimize(instance)


