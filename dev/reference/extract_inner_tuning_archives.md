# Extract Inner Tuning Archives

Extract inner tuning archives of nested resampling. Implemented for
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
and
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).
The function iterates over the
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
objects and binds the tuning archives to a
[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
must be initialized with `store_tuning_instance = TRUE` and
[`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html)
or
[`mlr3::benchmark()`](https://mlr3.mlr-org.com/reference/benchmark.html)
must be called with `store_models = TRUE`.

## Usage

``` r
extract_inner_tuning_archives(
  x,
  unnest = "x_domain",
  exclude_columns = "uhash"
)
```

## Arguments

- x:

  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  \|
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)).

- unnest:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Transforms list columns to separate columns. By default, `x_domain` is
  unnested. Set to `NULL` if no column should be unnested.

- exclude_columns:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Exclude columns from result table. Set to `NULL` if no column should
  be excluded.

## Value

[`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html).

## Data structure

The returned data table has the following columns:

- `experiment` (integer(1))  
  Index, giving the according row number in the original benchmark grid.

- `iteration` (integer(1))  
  Iteration of the outer resampling.

- One column for each hyperparameter of the search spaces.

- One column for each performance measure.

- `runtime_learners` (`numeric(1)`)  
  Sum of training and predict times logged in learners per
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  / evaluation. This does not include potential overhead time.

- `timestamp` (`POSIXct`)  
  Time stamp when the evaluation was logged into the archive.

- `batch_nr` (`integer(1)`)  
  Hyperparameters are evaluated in batches. Each batch has a unique
  batch number.

- `x_domain` ([`list()`](https://rdrr.io/r/base/list.html))  
  List of transformed hyperparameter values. By default this column is
  unnested.

- `x_domain_*` (`any`)  
  Separate column for each transformed hyperparameter.

- `resample_result`
  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html))  
  Resample result of the inner resampling.

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

# extract inner archives
extract_inner_tuning_archives(rr)
#>    iteration        cp classif.ce  x_domain_cp runtime_learners
#>        <int>     <num>      <num>        <num>            <num>
#> 1:         1 -4.604232       0.24 0.0100093854            0.007
#> 2:         1 -2.661451       0.24 0.0698467737            0.005
#> 3:         1 -5.003197       0.24 0.0067164406            0.005
#> 4:         1 -4.517196       0.24 0.0109195967            0.009
#> 5:         2 -7.445684       0.04 0.0005839564            0.007
#> 6:         2 -8.450607       0.04 0.0002137707            0.006
#> 7:         2 -5.097860       0.04 0.0061098084            0.006
#> 8:         2 -5.960192       0.04 0.0025794179            0.006
#>              timestamp warnings errors batch_nr  resample_result task_id
#>                 <POSc>    <int>  <int>    <int>           <list>  <char>
#> 1: 2025-12-14 15:24:38        0      0        1 <ResampleResult>    iris
#> 2: 2025-12-14 15:24:38        0      0        2 <ResampleResult>    iris
#> 3: 2025-12-14 15:24:39        0      0        3 <ResampleResult>    iris
#> 4: 2025-12-14 15:24:39        0      0        4 <ResampleResult>    iris
#> 5: 2025-12-14 15:24:39        0      0        1 <ResampleResult>    iris
#> 6: 2025-12-14 15:24:39        0      0        2 <ResampleResult>    iris
#> 7: 2025-12-14 15:24:39        0      0        3 <ResampleResult>    iris
#> 8: 2025-12-14 15:24:39        0      0        4 <ResampleResult>    iris
#>             learner_id resampling_id
#>                 <char>        <char>
#> 1: classif.rpart.tuned            cv
#> 2: classif.rpart.tuned            cv
#> 3: classif.rpart.tuned            cv
#> 4: classif.rpart.tuned            cv
#> 5: classif.rpart.tuned            cv
#> 6: classif.rpart.tuned            cv
#> 7: classif.rpart.tuned            cv
#> 8: classif.rpart.tuned            cv
```
