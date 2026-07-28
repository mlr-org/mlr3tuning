# Assertions for Callbacks

Assertions for
[CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)
class.
[bbotk::CallbackBatch](https://bbotk.mlr-org.com/reference/CallbackBatch.html)
objects are accepted as well since they work in batch tuning too.

## Usage

``` r
assert_batch_tuning_callback(callback, null_ok = FALSE)

assert_batch_tuning_callbacks(callbacks)
```

## Arguments

- callback:

  ([bbotk::CallbackBatch](https://bbotk.mlr-org.com/reference/CallbackBatch.html)
  \|
  [CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)).

- null_ok:

  (`logical(1)`)  
  If `TRUE`, `NULL` is allowed.

- callbacks:

  (list of
  [bbotk::CallbackBatch](https://bbotk.mlr-org.com/reference/CallbackBatch.html)
  \| list of
  [CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)).

## Value

[bbotk::CallbackBatch](https://bbotk.mlr-org.com/reference/CallbackBatch.html)
\|
[CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)
\| List of them.
