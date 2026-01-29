# Frozen Rush Data Storage

Freezes the Redis data base of an
[ArchiveAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveAsyncTuning.md)
to a
[`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html).
No further points can be added to the archive but the data can be
accessed and analyzed. Useful when the Redis data base is not
permanently available. Use the callback
[mlr3tuning.async_freeze_archive](https://mlr3tuning.mlr-org.com/dev/reference/mlr3tuning.async_freeze_archive.md)
to freeze the archive after the optimization has finished.

## S3 Methods

- `as.data.table(archive)`  
  ArchiveAsyncTuningFrozen -\>
  [`data.table::data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)  
  Returns a tabular view of all performed function calls of the
  Objective. The `x_domain` column is unnested to separate columns.

## Super classes

[`bbotk::Archive`](https://bbotk.mlr-org.com/reference/Archive.html) -\>
[`bbotk::ArchiveAsync`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html)
-\>
[`bbotk::ArchiveAsyncFrozen`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html)
-\> `ArchiveAsyncTuningFrozen`

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

- [`ArchiveAsyncTuningFrozen$new()`](#method-ArchiveAsyncTuningFrozen-new)

- [`ArchiveAsyncTuningFrozen$learner()`](#method-ArchiveAsyncTuningFrozen-learner)

- [`ArchiveAsyncTuningFrozen$learners()`](#method-ArchiveAsyncTuningFrozen-learners)

- [`ArchiveAsyncTuningFrozen$learner_param_vals()`](#method-ArchiveAsyncTuningFrozen-learner_param_vals)

- [`ArchiveAsyncTuningFrozen$predictions()`](#method-ArchiveAsyncTuningFrozen-predictions)

- [`ArchiveAsyncTuningFrozen$resample_result()`](#method-ArchiveAsyncTuningFrozen-resample_result)

- [`ArchiveAsyncTuningFrozen$print()`](#method-ArchiveAsyncTuningFrozen-print)

- [`ArchiveAsyncTuningFrozen$clone()`](#method-ArchiveAsyncTuningFrozen-clone)

Inherited methods

- [`bbotk::Archive$format()`](https://bbotk.mlr-org.com/reference/Archive.html#method-format)
- [`bbotk::Archive$help()`](https://bbotk.mlr-org.com/reference/Archive.html#method-help)
- [`bbotk::ArchiveAsync$best()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-best)
- [`bbotk::ArchiveAsync$nds_selection()`](https://bbotk.mlr-org.com/reference/ArchiveAsync.html#method-nds_selection)
- [`bbotk::ArchiveAsyncFrozen$clear()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-clear)
- [`bbotk::ArchiveAsyncFrozen$data_with_state()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-data_with_state)
- [`bbotk::ArchiveAsyncFrozen$pop_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-pop_point)
- [`bbotk::ArchiveAsyncFrozen$push_failed_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-push_failed_point)
- [`bbotk::ArchiveAsyncFrozen$push_points()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-push_points)
- [`bbotk::ArchiveAsyncFrozen$push_result()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-push_result)
- [`bbotk::ArchiveAsyncFrozen$push_running_point()`](https://bbotk.mlr-org.com/reference/ArchiveAsyncFrozen.html#method-push_running_point)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    ArchiveAsyncTuningFrozen$new(archive)

#### Arguments

- `archive`:

  ([ArchiveAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/ArchiveAsyncTuning.md))  
  The archive to freeze.

------------------------------------------------------------------------

### Method `learner()`

Retrieve
[mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html) of the
i-th evaluation, by position or by unique hash `uhash`. `i` and `uhash`
are mutually exclusive. Learner does not contain a model. Use
`$learners()` to get learners with models.

#### Usage

    ArchiveAsyncTuningFrozen$learner(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuningFrozen$learners(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuningFrozen$learner_param_vals(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuningFrozen$predictions(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuningFrozen$resample_result(i = NULL, uhash = NULL)

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

    ArchiveAsyncTuningFrozen$print()

#### Arguments

- `...`:

  (ignored).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ArchiveAsyncTuningFrozen$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
