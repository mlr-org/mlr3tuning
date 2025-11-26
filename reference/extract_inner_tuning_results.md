# Extract Inner Tuning Results

Extract inner tuning results of nested resampling. Implemented for
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
and
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).

## Usage

``` r
extract_inner_tuning_results(x, tuning_instance, ...)

# S3 method for class 'ResampleResult'
extract_inner_tuning_results(x, tuning_instance = FALSE, ...)

# S3 method for class 'BenchmarkResult'
extract_inner_tuning_results(x, tuning_instance = FALSE, ...)
```

## Arguments

- x:

  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  \|
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)).

- tuning_instance:

  (`logical(1)`)  
  If `TRUE`, tuning instances are added to the table.

- ...:

  (any)  
  Additional arguments.

## Value

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

## Details

The function iterates over the
[AutoTuner](https://mlr3tuning.mlr-org.com/reference/AutoTuner.md)
objects and binds the tuning results to a
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
The [AutoTuner](https://mlr3tuning.mlr-org.com/reference/AutoTuner.md)
must be initialized with `store_tuning_instance = TRUE` and
[`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html)
or
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html)
must be called with `store_models = TRUE`. Optionally, the tuning
instance can be added for each iteration.

## Data structure

The returned data table has the following columns:

- `experiment` (integer(1))  
  Index, giving the according row number in the original benchmark grid.

- `iteration` (integer(1))  
  Iteration of the outer resampling.

- One column for each hyperparameter of the search spaces.

- One column for each performance measure.

- `learner_param_vals` ([`list()`](https://rdrr.io/r/base/list.html))  
  Hyperparameter values used by the learner. Includes fixed and proposed
  hyperparameter values.

- `x_domain` ([`list()`](https://rdrr.io/r/base/list.html))  
  List of transformed hyperparameter values.

- `tuning_instance`
  ([TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
  \|
  [TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md))  
  Optionally, tuning instances.

- `task_id` (`character(1)`).

- `learner_id` (`character(1)`).

- `resampling_id` (`character(1)`).

## Examples

``` r
# Nested Resampling on Palmer Penguins Data Set

learner = lrn("classif.rpart",
  cp = to_tune(1e-04, 1e-1, logscale = TRUE))

# create auto tuner
at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

resampling_outer = rsmp("cv", folds = 2)
rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)

# extract inner results
extract_inner_tuning_results(rr)
#>    iteration        cp classif.ce learner_param_vals  x_domain task_id
#>        <int>     <num>      <num>             <list>    <list>  <char>
#> 1:         1 -3.259746       0.04          <list[2]> <list[1]>    iris
#> 2:         2 -8.744416       0.00          <list[2]> <list[1]>    iris
#>             learner_id resampling_id
#>                 <char>        <char>
#> 1: classif.rpart.tuned            cv
#> 2: classif.rpart.tuned            cv
```
