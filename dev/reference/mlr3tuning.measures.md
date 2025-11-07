# Measure Callback

This
[mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html)
scores the hyperparameter configurations on additional measures while
tuning. Usually, the configurations can be scored on additional measures
after tuning (see
[ArchiveBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveBatchTuning.md)).
However, if the memory is not sufficient to store the
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html),
it is necessary to score the additional measures while tuning. The
measures are not taken into account by the tuner.

## Examples

``` r
clbk("mlr3tuning.measures")
#> <CallbackBatchTuning:mlr3tuning.measures>: Additional Measures Callback
#> * Active Stages: on_eval_before_archive, on_optimization_begin

# additionally score the configurations on the accuracy measure
instance = tune(
  tuner = tnr("random_search", batch_size = 2),
  task = tsk("pima"),
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  term_evals = 4,
  callbacks = clbk("mlr3tuning.measures", measures = msr("classif.acc"))
)
```
