# Asynchronous Tuning Callback

Specialized
[bbotk::CallbackAsync](https://bbotk.mlr-org.com/reference/CallbackAsync.html)
for asynchronous tuning. Callbacks allow to customize the behavior of
processes in mlr3tuning. The
[`callback_async_tuning()`](https://mlr3tuning.mlr-org.com/dev/reference/callback_async_tuning.md)
function creates a CallbackAsyncTuning. Predefined callbacks are stored
in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_callbacks](https://mlr3misc.mlr-org.com/reference/mlr_callbacks.html)
and can be retrieved with
[`clbk()`](https://mlr3misc.mlr-org.com/reference/clbk.html). For more
information on tuning callbacks see
[`callback_async_tuning()`](https://mlr3tuning.mlr-org.com/dev/reference/callback_async_tuning.md).

## Super classes

[`mlr3misc::Callback`](https://mlr3misc.mlr-org.com/reference/Callback.html)
-\>
[`bbotk::CallbackAsync`](https://bbotk.mlr-org.com/reference/CallbackAsync.html)
-\> `CallbackAsyncTuning`

## Public fields

- `on_eval_after_xs`:

  (`function()`)  
  Stage called after xs is passed. Called in
  `ObjectiveTuningAsync$eval()`.

- `on_resample_begin`:

  (`function()`)  
  Stage called at the beginning of an evaluation. Called in
  `workhorse()` (internal).

- `on_resample_before_train`:

  (`function()`)  
  Stage called before training the learner. Called in `workhorse()`
  (internal).

- `on_resample_before_predict`:

  (`function()`)  
  Stage called before predicting. Called in `workhorse()` (internal).

- `on_resample_end`:

  (`function()`)  
  Stage called at the end of an evaluation. Called in `workhorse()`
  (internal).

- `on_eval_after_resample`:

  (`function()`)  
  Stage called after hyperparameter configurations are evaluated. Called
  in `ObjectiveTuningAsync$eval()`.

- `on_eval_before_archive`:

  (`function()`)  
  Stage called before performance values are written to the archive.
  Called in `ObjectiveTuningAsync$eval()`.

- `on_tuning_result_begin`:

  (`function()`)  
  Stage called before the results are written. Called in
  `TuningInstance*$assign_result()`.

## Methods

### Public methods

- [`CallbackAsyncTuning$clone()`](#method-CallbackAsyncTuning-clone)

Inherited methods

- [`mlr3misc::Callback$call()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-call)
- [`mlr3misc::Callback$format()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-format)
- [`mlr3misc::Callback$help()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-help)
- [`mlr3misc::Callback$initialize()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-initialize)
- [`mlr3misc::Callback$print()`](https://mlr3misc.mlr-org.com/reference/Callback.html#method-print)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CallbackAsyncTuning$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
