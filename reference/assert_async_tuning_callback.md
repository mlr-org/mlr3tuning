# Assertions for Callbacks

Assertions for
[CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/reference/CallbackAsyncTuning.md)
class.
[bbotk::CallbackAsync](https://bbotk.mlr-org.com/reference/CallbackAsync.html)
objects are accepted as well since they work in asynchronous tuning too.

## Usage

``` r
assert_async_tuning_callback(callback, null_ok = FALSE)

assert_async_tuning_callbacks(callbacks)
```

## Arguments

- callback:

  ([bbotk::CallbackAsync](https://bbotk.mlr-org.com/reference/CallbackAsync.html)
  \|
  [CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/reference/CallbackAsyncTuning.md)).

- null_ok:

  (`logical(1)`)  
  If `TRUE`, `NULL` is allowed.

- callbacks:

  (list of
  [bbotk::CallbackAsync](https://bbotk.mlr-org.com/reference/CallbackAsync.html)
  \| list of
  [CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/reference/CallbackAsyncTuning.md)).

## Value

[bbotk::CallbackAsync](https://bbotk.mlr-org.com/reference/CallbackAsync.html)
\|
[CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/reference/CallbackAsyncTuning.md)
\| List of them.
