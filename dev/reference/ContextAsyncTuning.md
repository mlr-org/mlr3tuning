# Asynchronous Tuning Context

A
[CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackAsyncTuning.md)
accesses and modifies data during the optimization via the
`ContextAsyncTuning`. See the section on active bindings for a list of
modifiable objects. See
[`callback_async_tuning()`](https://mlr3tuning.mlr-org.com/dev/reference/callback_async_tuning.md)
for a list of stages that access `ContextAsyncTuning`.

## Details

Changes to `$instance` and `$optimizer` in the stages executed on the
workers are not reflected in the main process.

## Super classes

[`mlr3misc::Context`](https://mlr3misc.mlr-org.com/reference/Context.html)
-\>
[`bbotk::ContextAsync`](https://bbotk.mlr-org.com/reference/ContextAsync.html)
-\> `ContextAsyncTuning`

## Active bindings

- `xs_learner`:

  (list())  
  The hyperparameter configuration currently evaluated. Contains the
  values on the learner scale i.e. transformations are applied.

- `resample_result`:

  ([mlr3::BenchmarkResult](https://mlr3.mlr-org.com/reference/BenchmarkResult.html))  
  The resample result of the hyperparameter configuration currently
  evaluated.

- `aggregated_performance`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  Aggregated performance scores and training time of the evaluated
  hyperparameter configuration. This list is passed to the archive. A
  callback can add additional elements which are also written to the
  archive.

- `result_learner_param_vals`:

  (list())  
  The learner parameter values passed to `instance$assign_result()`.

## Methods

### Public methods

- [`ContextAsyncTuning$clone()`](#method-ContextAsyncTuning-clone)

Inherited methods

- [`mlr3misc::Context$format()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-format)
- [`mlr3misc::Context$print()`](https://mlr3misc.mlr-org.com/reference/Context.html#method-print)
- [`bbotk::ContextAsync$initialize()`](https://bbotk.mlr-org.com/reference/ContextAsync.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ContextAsyncTuning$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
