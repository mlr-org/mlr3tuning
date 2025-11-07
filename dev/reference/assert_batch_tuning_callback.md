# Assertions for Callbacks

Assertions for
[CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)
class.

## Usage

``` r
assert_batch_tuning_callback(callback, null_ok = FALSE)

assert_batch_tuning_callbacks(callbacks)
```

## Arguments

- callback:

  ([CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)).

- null_ok:

  (`logical(1)`)  
  If `TRUE`, `NULL` is allowed.

- callbacks:

  (list of
  [CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)).

## Value

\[CallbackBatchTuning \| List of
[CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md)s.
