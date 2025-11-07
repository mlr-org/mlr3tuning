# Batch Tuning Context

A
[CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)
accesses and modifies data during the optimization via the
`ContextBatchTuning`. See the section on active bindings for a list of
modifiable objects. See
[`callback_batch_tuning()`](https://mlr3tuning.mlr-org.com/dev/reference/callback_batch_tuning.md)
for a list of stages that access `ContextBatchTuning`.

## Super classes

[`mlr3misc::Context`](https://mlr3misc.mlr-org.com/reference/Context.html)
-\>
[`bbotk::ContextBatch`](https://bbotk.mlr-org.com/reference/ContextBatch.html)
-\> `ContextBatchTuning`

## Active bindings

- `xss`:

  (list())  
  The hyperparameter configurations of the latest batch. Contains the
  values on the learner scale i.e. transformations are applied. See
  `$xdt` for the untransformed values.

- `design`:

  ([data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  The benchmark design of the latest batch.

- `benchmark_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  The benchmark result of the latest batch.

- `aggregated_performance`:

  ([data.table::data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html))  
  Aggregated performance scores and training time of the latest batch.
  This data table is passed to the archive. A callback can add
  additional columns which are also written to the archive.

- `result_learner_param_vals`:

  (list())  
  The learner parameter values passed to `instance$assign_result()`.

## Methods

### Public methods

- [`ContextBatchTuning$clone()`](#method-ContextBatchTuning-clone)

Inherited methods

- [`mlr3misc::Context$format()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-format)
- [`mlr3misc::Context$print()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-print)
- [`bbotk::ContextBatch$initialize()`](https://bbotk.mlr-org.com/reference/ContextBatch.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ContextBatchTuning$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
