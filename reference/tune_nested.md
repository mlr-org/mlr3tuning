# Function for Nested Resampling

Function to conduct nested resampling.

## Usage

``` r
tune_nested(
  tuner,
  task,
  learner,
  inner_resampling,
  outer_resampling,
  measure = NULL,
  term_evals = NULL,
  term_time = NULL,
  terminator = NULL,
  search_space = NULL,
  store_tuning_instance = TRUE,
  store_benchmark_result = TRUE,
  store_models = FALSE,
  check_values = FALSE,
  callbacks = NULL
)
```

## Arguments

- tuner:

  ([Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md))  
  Optimization algorithm.

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))  
  Task to operate on.

- learner:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  Learner to tune.

- inner_resampling:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling used for the inner loop.

- outer_resampling:

  [mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))  
  Resampling used for the outer loop.

- measure:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
  Measure to optimize. If `NULL`, default measure is used.

- term_evals:

  (`integer(1)`)  
  Number of allowed evaluations. Ignored if `terminator` is passed.

- term_time:

  (`integer(1)`)  
  Maximum allowed time in seconds. Ignored if `terminator` is passed.

- terminator:

  ([bbotk::Terminator](https://bbotk.mlr-org.com/reference/Terminator.html))  
  Stop criterion of the tuning process.

- search_space:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Hyperparameter search space. If `NULL` (default), the search space is
  constructed from the
  [paradox::TuneToken](https://paradox.mlr-org.com/reference/to_tune.html)
  of the learner's parameter set (learner\$param_set).

- store_tuning_instance:

  (`logical(1)`)  
  If `TRUE` (default), stores the internally created
  [TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
  with all intermediate results in slot `$tuning_instance`.

- store_benchmark_result:

  (`logical(1)`)  
  If `TRUE` (default), store resample result of evaluated hyperparameter
  configurations in archive as
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).

- store_models:

  (`logical(1)`)  
  If `TRUE`, fitted models are stored in the benchmark result
  (`archive$benchmark_result`). If `store_benchmark_result = FALSE`,
  models are only stored temporarily and not accessible after the
  tuning. This combination is needed for measures that require a model.

- check_values:

  (`logical(1)`)  
  If `TRUE`, hyperparameter values are checked before evaluation and
  performance scores after. If `FALSE` (default), values are unchecked
  but computational overhead is reduced.

- callbacks:

  (list of
  [mlr3misc::Callback](https://mlr3misc.mlr-org.com/reference/Callback.html))  
  List of callbacks.

## Value

[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)

## Examples

``` r
# Nested resampling on Palmer Penguins data set
rr = tune_nested(
  tuner = tnr("random_search", batch_size = 2),
  task = tsk("penguins"),
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
  inner_resampling = rsmp ("holdout"),
  outer_resampling = rsmp("cv", folds = 2),
  measure = msr("classif.ce"),
  term_evals = 2)

# Performance scores estimated on the outer resampling
rr$score()
#>     task_id          learner_id resampling_id iteration classif.ce
#>      <char>              <char>        <char>     <int>      <num>
#> 1: penguins classif.rpart.tuned            cv         1 0.05813953
#> 2: penguins classif.rpart.tuned            cv         2 0.07558140
#> Hidden columns: task, learner, resampling, prediction_test

# Unbiased performance of the final model trained on the full data set
rr$aggregate()
#> classif.ce 
#> 0.06686047 
```
