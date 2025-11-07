# MLflow Connector Callback

This
[mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)
logs the hyperparameter configurations and the performance of the
configurations to MLflow.

## Examples

``` r
clbk("mlr3tuning.async_mlflow", tracking_uri = "http://localhost:5000")
#> <CallbackAsyncTuning:mlr3tuning.async_mlflow>: MLflow Connector
#> * Active Stages: on_eval_before_archive, on_eval_after_xs,
#>   on_optimization_begin

if (FALSE) { # \dontrun{
rush::rush_plan(n_workers = 4)

learner = lrn("classif.rpart",
  minsplit = to_tune(2, 128),
  cp = to_tune(1e-04, 1e-1))

instance = TuningInstanceAsyncSingleCrit$new(
  task = tsk("pima"),
  learner = learner,
  resampling = rsmp("cv", folds = 3),
  measure = msr("classif.ce"),
  terminator = trm("evals", n_evals = 20),
  store_benchmark_result = FALSE,
  callbacks = clbk("mlr3tuning.rush_mlflow", tracking_uri = "http://localhost:8080")
)

tuner = tnr("random_search_v2")
tuner$optimize(instance)
} # }
```
