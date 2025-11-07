# Assertions for Callbacks

Assertions for
[CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackAsyncTuning.md)
class.

## Usage

``` r
assert_async_tuning_callback(callback, null_ok = FALSE)

assert_async_tuning_callbacks(callbacks)
```

## Arguments

- callback:

  ([CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackAsyncTuning.md)).

- null_ok:

  (`logical(1)`)  
  If `TRUE`, `NULL` is allowed.

- callbacks:

  (list of
  [CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackAsyncTuning.md)).

## Value

\[CallbackAsyncTuning \| List of
[CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackAsyncTuning.md)s.
