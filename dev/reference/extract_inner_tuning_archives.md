# Extract Inner Tuning Archives

Extract inner tuning archives of nested resampling. Implemented for
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
and
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).
The function iterates over the
[AutoTuner](https://mlr3tuning.mlr-org.com/dev/reference/AutoTuner.md)
objects and binds the tuning archives to a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
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

[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

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
#>    iteration        cp classif.ce x_domain_cp runtime_learners
#>        <int>     <num>      <num>       <num>            <num>
#> 1:         1 -2.771268       0.08 0.062582599            0.006
#> 2:         1 -5.852816       0.08 0.002871801            0.006
#> 3:         1 -6.365882       0.08 0.001719224            0.006
#> 4:         1 -3.185002       0.08 0.041378177            0.006
#> 5:         2 -6.819407       0.04 0.001092369            0.006
#> 6:         2 -6.361894       0.04 0.001726095            0.006
#> 7:         2 -5.017906       0.04 0.006618373            0.005
#> 8:         2 -4.487537       0.04 0.011248315            0.005
#>              timestamp warnings errors batch_nr  resample_result task_id
#>                 <POSc>    <int>  <int>    <int>           <list>  <char>
#> 1: 2026-03-18 15:48:53        0      0        1 <ResampleResult>    iris
#> 2: 2026-03-18 15:48:53        0      0        2 <ResampleResult>    iris
#> 3: 2026-03-18 15:48:53        0      0        3 <ResampleResult>    iris
#> 4: 2026-03-18 15:48:53        0      0        4 <ResampleResult>    iris
#> 5: 2026-03-18 15:48:52        0      0        1 <ResampleResult>    iris
#> 6: 2026-03-18 15:48:52        0      0        2 <ResampleResult>    iris
#> 7: 2026-03-18 15:48:52        0      0        3 <ResampleResult>    iris
#> 8: 2026-03-18 15:48:52        0      0        4 <ResampleResult>    iris
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
