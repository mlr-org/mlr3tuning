# Backup Benchmark Result Callback

This
[mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)
writes the
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
after each batch to disk.

## Examples

``` r
clbk("mlr3tuning.backup", path = "backup.rds")
#> <CallbackBatchTuning:mlr3tuning.backup>: Backup Benchmark Result Callback
#> * Active Stages: on_optimizer_after_eval, on_optimization_begin

# tune classification tree on the pima data set
instance = tune(
  tuner = tnr("random_search", batch_size = 2),
  task = tsk("pima"),
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  term_evals = 4,
  callbacks = clbk("mlr3tuning.backup", path = tempfile(fileext = ".rds"))
)
```
