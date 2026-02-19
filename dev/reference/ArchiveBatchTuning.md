# Class for Logging Evaluated Hyperparameter Configurations

The `ArchiveBatchTuning` stores all evaluated hyperparameter
configurations and performance scores in a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).

## Details

The ArchiveBatchTuning is a container around a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
Each row corresponds to a single evaluation of a hyperparameter
configuration. See the section on Data Structure for more information.
The archive stores additionally a
[mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html)
(`$benchmark_result`) that records the resampling experiments. Each
experiment corresponds to a single evaluation of a hyperparameter
configuration. The table (`$data`) and the benchmark result
(`$benchmark_result`) are linked by the `uhash` column. If the archive
is passed to
[`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html),
both are joined automatically.

## Data Structure

The table (`$data`) has the following columns:

- One column for each hyperparameter of the search space
  (`$search_space`).

- One (list-)column for the `internal_tuned_values`

- One column for each performance measure (`$codomain`).

- `x_domain` ([`list()`](https://rdrr.io/r/base/list.html))  
  Lists of (transformed) hyperparameter values that are passed to the
  learner.

- `runtime_learners` (`numeric(1)`)  
  Sum of training and predict times logged in learners per
  [mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
  / evaluation. This does not include potential overhead time.

- `timestamp` (`POSIXct`)  
  Time stamp when the evaluation was logged into the archive.

- `batch_nr` (`integer(1)`)  
  Hyperparameters are evaluated in batches. Each batch has a unique
  batch number.

- `uhash` (`character(1)`)  
  Connects each hyperparameter configuration to the resampling
  experiment stored in the
  [mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html).

## Analysis

For analyzing the tuning results, it is recommended to pass the
ArchiveBatchTuning to
[`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).
The returned data table is joined with the benchmark result which adds
the
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
for each hyperparameter evaluation.

The archive provides various getters (e.g. `$learners()`) to ease the
access. All getters extract by position (`i`) or unique hash (`uhash`).
For a complete list of all getters see the methods section.

The benchmark result (`$benchmark_result`) allows to score the
hyperparameter configurations again on a different measure.
Alternatively, measures can be supplied to
[`as.data.table()`](https://rdrr.io/pkg/data.table/man/as.data.table.html).

The [mlr3viz](https://CRAN.R-project.org/package=mlr3viz) package
provides visualizations for tuning results.

## S3 Methods

- `as.data.table.ArchiveTuning(x, unnest = "x_domain", exclude_columns = "uhash", measures = NULL)`  
  Returns a tabular view of all evaluated hyperparameter
  configurations.  
  ArchiveBatchTuning -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  

  - `x` (ArchiveBatchTuning)

  - `unnest` ([`character()`](https://rdrr.io/r/base/character.html))  
    Transforms list columns to separate columns. Set to `NULL` if no
    column should be unnested.

  - `exclude_columns`
    ([`character()`](https://rdrr.io/r/base/character.html))  
    Exclude columns from table. Set to `NULL` if no column should be
    excluded.

  - `measures` (List of
    [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))  
    Score hyperparameter configurations on additional measures.

## Super classes

[`bbotk::Archive`](https://bbotk.mlr-org.com/reference/Archive.html) -\>
[`bbotk::ArchiveBatch`](https://bbotk.mlr-org.com/reference/ArchiveBatch.html)
-\> `ArchiveBatchTuning`

## Public fields

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  Benchmark result.

## Active bindings

- `internal_search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  The search space containing those parameters that are internally
  optimized by the
  [`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html).

## Methods

### Public methods

- [`ArchiveBatchTuning$new()`](#method-ArchiveBatchTuning-new)

- [`ArchiveBatchTuning$learner()`](#method-ArchiveBatchTuning-learner)

- [`ArchiveBatchTuning$learners()`](#method-ArchiveBatchTuning-learners)

- [`ArchiveBatchTuning$learner_param_vals()`](#method-ArchiveBatchTuning-learner_param_vals)

- [`ArchiveBatchTuning$predictions()`](#method-ArchiveBatchTuning-predictions)

- [`ArchiveBatchTuning$resample_result()`](#method-ArchiveBatchTuning-resample_result)

- [`ArchiveBatchTuning$print()`](#method-ArchiveBatchTuning-print)

- [`ArchiveBatchTuning$clone()`](#method-ArchiveBatchTuning-clone)

Inherited methods

- [`bbotk::Archive$format()`](https://bbotk.mlr-org.com/reference/Archive.html#method-format)
- [`bbotk::Archive$help()`](https://bbotk.mlr-org.com/reference/Archive.html#method-help)
- [`bbotk::ArchiveBatch$add_evals()`](https://bbotk.mlr-org.com/reference/ArchiveBatch.html#method-add_evals)
- [`bbotk::ArchiveBatch$best()`](https://bbotk.mlr-org.com/reference/ArchiveBatch.html#method-best)
- [`bbotk::ArchiveBatch$clear()`](https://bbotk.mlr-org.com/reference/ArchiveBatch.html#method-clear)
- [`bbotk::ArchiveBatch$nds_selection()`](https://bbotk.mlr-org.com/reference/ArchiveBatch.html#method-nds_selection)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ArchiveBatchTuning$new(
      search_space,
      codomain,
      check_values = FALSE,
      internal_search_space = NULL
    )

#### Arguments

- `search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  Hyperparameter search space. If `NULL` (default), the search space is
  constructed from the
  [paradox::TuneToken](https://paradox.mlr-org.com/reference/to_tune.html)
  of the learner's parameter set (learner\$param_set). When using
  [`to_tune()`](https://paradox.mlr-org.com/reference/to_tune.html)
  tokens, dependencies for hierarchical search spaces are automatically
  handled.

- `codomain`:

  ([bbotk::Codomain](https://bbotk.mlr-org.com/reference/Codomain.html))  
  Specifies codomain of objective function i.e. a set of performance
  measures. Internally created from provided
  [mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html)s.

- `check_values`:

  (`logical(1)`)  
  If `TRUE` (default), hyperparameter configurations are checked for
  validity.

- `internal_search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  or `NULL`)  
  The internal search space.

------------------------------------------------------------------------

### Method `learner()`

Retrieve
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) of the
i-th evaluation, by position or by unique hash `uhash`. `i` and `uhash`
are mutually exclusive. Learner does not contain a model. Use
`$learners()` to get learners with models.

#### Usage

    ArchiveBatchTuning$learner(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method `learners()`

Retrieve list of trained
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) objects
of the i-th evaluation, by position or by unique hash `uhash`. `i` and
`uhash` are mutually exclusive.

#### Usage

    ArchiveBatchTuning$learners(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method `learner_param_vals()`

Retrieve param values of the i-th evaluation, by position or by unique
hash `uhash`. `i` and `uhash` are mutually exclusive.

#### Usage

    ArchiveBatchTuning$learner_param_vals(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method `predictions()`

Retrieve list of
[mlr3::Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)
objects of the i-th evaluation, by position or by unique hash `uhash`.
`i` and `uhash` are mutually exclusive.

#### Usage

    ArchiveBatchTuning$predictions(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method `resample_result()`

Retrieve
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
of the i-th evaluation, by position or by unique hash `uhash`. `i` and
`uhash` are mutually exclusive.

#### Usage

    ArchiveBatchTuning$resample_result(i = NULL, uhash = NULL)

#### Arguments

- `i`:

  (`integer(1)`)  
  The iteration value to filter for.

- `uhash`:

  (`logical(1)`)  
  The `uhash` value to filter for.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Printer.

#### Usage

    ArchiveBatchTuning$print()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ArchiveBatchTuning$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
