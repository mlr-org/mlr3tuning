# Rush Data Storage

The \`ArchiveAsyncTuning“ stores all evaluated hyperparameter
configurations and performance scores in a
[rush::Rush](https://rush.mlr-org.com/reference/Rush.html) database.

## Details

The ArchiveAsyncTuning is a connector to a
[rush::Rush](https://rush.mlr-org.com/reference/Rush.html) database.

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

## Analysis

For analyzing the tuning results, it is recommended to pass the
ArchiveAsyncTuning to
[`as.data.table()`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html).
The returned data table contains the
[mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html)
for each hyperparameter evaluation.

## S3 Methods

- `as.data.table.ArchiveTuning(x, unnest = "x_domain", exclude_columns = "uhash", measures = NULL)`  
  Returns a tabular view of all evaluated hyperparameter
  configurations.  
  ArchiveAsyncTuning -\>
  [`data.table::data.table()`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)  

  - `x` (ArchiveAsyncTuning)

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
[`bbotk::ArchiveAsync`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html)
-\> `ArchiveAsyncTuning`

## Active bindings

- `internal_search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html))  
  The search space containing those parameters that are internally
  optimized by the
  [`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html).

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  Benchmark result.

## Methods

### Public methods

- [`ArchiveAsyncTuning$new()`](#method-ArchiveAsyncTuning-new)

- [`ArchiveAsyncTuning$learner()`](#method-ArchiveAsyncTuning-learner)

- [`ArchiveAsyncTuning$learners()`](#method-ArchiveAsyncTuning-learners)

- [`ArchiveAsyncTuning$learner_param_vals()`](#method-ArchiveAsyncTuning-learner_param_vals)

- [`ArchiveAsyncTuning$predictions()`](#method-ArchiveAsyncTuning-predictions)

- [`ArchiveAsyncTuning$resample_result()`](#method-ArchiveAsyncTuning-resample_result)

- [`ArchiveAsyncTuning$print()`](#method-ArchiveAsyncTuning-print)

- [`ArchiveAsyncTuning$clone()`](#method-ArchiveAsyncTuning-clone)

Inherited methods

- [`bbotk::Archive$format()`](https://bbotk.mlr-org.com/reference/Archive.html#method-format)
- [`bbotk::Archive$help()`](https://bbotk.mlr-org.com/reference/Archive.html#method-help)
- [`bbotk::ArchiveAsync$best()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-best)
- [`bbotk::ArchiveAsync$clear()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-clear)
- [`bbotk::ArchiveAsync$data_with_state()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-data_with_state)
- [`bbotk::ArchiveAsync$nds_selection()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-nds_selection)
- [`bbotk::ArchiveAsync$pop_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-pop_point)
- [`bbotk::ArchiveAsync$push_failed_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-push_failed_point)
- [`bbotk::ArchiveAsync$push_points()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-push_points)
- [`bbotk::ArchiveAsync$push_result()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-push_result)
- [`bbotk::ArchiveAsync$push_running_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-push_running_point)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ArchiveAsyncTuning$new(
      search_space,
      codomain,
      rush,
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

- `rush`:

  (`Rush`)  
  If a rush instance is supplied, the tuning runs without batches.

- `internal_search_space`:

  ([paradox::ParamSet](https://paradox.mlr-org.com/reference/ParamSet.html)
  or `NULL`)  
  The internal search space.

- `check_values`:

  (`logical(1)`)  
  If `TRUE` (default), hyperparameter configurations are check for
  validity.

------------------------------------------------------------------------

### Method `learner()`

Retrieve
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) of the
i-th evaluation, by position or by unique hash `uhash`. `i` and `uhash`
are mutually exclusive. Learner does not contain a model. Use
`$learners()` to get learners with models.

#### Usage

    ArchiveAsyncTuning$learner(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuning$learners(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuning$learner_param_vals(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuning$predictions(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuning$resample_result(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuning$print()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ArchiveAsyncTuning$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
