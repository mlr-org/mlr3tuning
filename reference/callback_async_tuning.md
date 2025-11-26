# Create Asynchronous Tuning Callback

Function to create a
[CallbackAsyncTuning](https://mlr3tuning.mlr-org.com/reference/CallbackAsyncTuning.md).
Predefined callbacks are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_callbacks](https://mlr3misc.mlr-org.com/reference/mlr_callbacks.html)
and can be retrieved with
[`clbk()`](https://mlr3misc.mlr-org.com/reference/clbk.html).

Tuning callbacks can be called from different stages of the tuning
process. The stages are prefixed with `on_*`.

    Start Tuning
         - on_optimization_begin
        Start Worker
             - on_worker_begin
             Start Optimization on Worker
               - on_optimizer_before_eval / on_optimizer_queue_before_eval
                 Start Evaluation
                   - on_eval_after_xs
                     Start Resampling Iteration
                       - on_resample_begin
                       - on_resample_before_train
                       - on_resample_before_predict
                       - on_resample_end
                     End Resampling Iteration
                   - on_eval_after_resample
                   - on_eval_before_archive
                 End Evaluation
              - on_optimizer_after_eval / on_optimizer_queue_after_eval
             End Optimization on Worker
             - on_worker_end
        End Worker
         - on_tuning_result_begin
         - on_result_begin
         - on_result_end
         - on_optimization_end
    End Tuning

See also the section on parameters for more information on the stages. A
tuning callback works with
[ContextAsyncTuning](https://mlr3tuning.mlr-org.com/reference/ContextAsyncTuning.md).

## Usage

``` r
callback_async_tuning(
  id,
  label = NA_character_,
  man = NA_character_,
  on_optimizer_queue_before_eval = NULL,
  on_optimization_begin = NULL,
  on_worker_begin = NULL,
  on_optimizer_before_eval = NULL,
  on_eval_after_xs = NULL,
  on_resample_begin = NULL,
  on_resample_before_train = NULL,
  on_resample_before_predict = NULL,
  on_resample_end = NULL,
  on_eval_after_resample = NULL,
  on_eval_before_archive = NULL,
  on_optimizer_after_eval = NULL,
  on_optimizer_queue_after_eval = NULL,
  on_worker_end = NULL,
  on_tuning_result_begin = NULL,
  on_result_begin = NULL,
  on_result_end = NULL,
  on_result = NULL,
  on_optimization_end = NULL
)
```

## Arguments

- id:

  (`character(1)`)  
  Identifier for the new instance.

- label:

  (`character(1)`)  
  Label for the new instance.

- man:

  (`character(1)`)  
  String in the format `[pkg]::[topic]` pointing to a manual page for
  this object. The referenced help package can be opened via method
  `$help()`.

- on_optimizer_queue_before_eval:

  (`function()`)  
  Stage called before the optimizer queue is evaluated. Called in
  `OptimInstance$.eval_queue()`. The functions must have two arguments
  named `callback` and `context`.

- on_optimization_begin:

  (`function()`)  
  Stage called at the beginning of the optimization. Called in
  `Optimizer$optimize()`. The functions must have two arguments named
  `callback` and `context`.

- on_worker_begin:

  (`function()`)  
  Stage called at the beginning of the optimization on the worker.
  Called in the worker loop. The functions must have two arguments named
  `callback` and `context`.

- on_optimizer_before_eval:

  (`function()`)  
  Stage called after the optimizer proposes points. Called in
  `OptimInstance$.eval_point()`. The functions must have two arguments
  named `callback` and `context`. The argument of
  `instance$.eval_point(xs)` and `xs_trafoed` and `extra` are available
  in the `context`. Or `xs` and `xs_trafoed` of `instance$.eval_queue()`
  are available in the `context`.

- on_eval_after_xs:

  (`function()`)  
  Stage called after xs is passed to the objective. Called in
  `ObjectiveTuningAsync$eval()`. The functions must have two arguments
  named `callback` and `context`. The argument of `$.eval(xs)` is
  available in the `context`.

- on_resample_begin:

  (`function()`)  
  Stage called at the beginning of a resampling iteration. Called in
  `workhorse()` (internal). See also
  [`mlr3::callback_resample()`](https://mlr3.mlr-org.com/reference/callback_resample.html).
  The functions must have two arguments named `callback` and `context`.

- on_resample_before_train:

  (`function()`)  
  Stage called before training the learner. Called in `workhorse()`
  (internal). See also
  [`mlr3::callback_resample()`](https://mlr3.mlr-org.com/reference/callback_resample.html).
  The functions must have two arguments named `callback` and `context`.

- on_resample_before_predict:

  (`function()`)  
  Stage called before predicting. Called in `workhorse()` (internal).
  See also
  [`mlr3::callback_resample()`](https://mlr3.mlr-org.com/reference/callback_resample.html).
  The functions must have two arguments named `callback` and `context`.

- on_resample_end:

  (`function()`)  
  Stage called at the end of a resampling iteration. Called in
  `workhorse()` (internal). See also
  [`mlr3::callback_resample()`](https://mlr3.mlr-org.com/reference/callback_resample.html).
  The functions must have two arguments named `callback` and `context`.

- on_eval_after_resample:

  (`function()`)  
  Stage called after a hyperparameter configuration is evaluated. Called
  in `ObjectiveTuningAsync$eval()`. The functions must have two
  arguments named `callback` and `context`. The `resample_result` is
  available in the \`context

- on_eval_before_archive:

  (`function()`)  
  Stage called before performance values are written to the archive.
  Called in `ObjectiveTuningAsync$eval()`. The functions must have two
  arguments named `callback` and `context`. The `aggregated_performance`
  is available in `context`.

- on_optimizer_after_eval:

  (`function()`)  
  Stage called after points are evaluated. Called in
  `OptimInstance$.eval_point()`. The functions must have two arguments
  named `callback` and `context`.

- on_optimizer_queue_after_eval:

  (`function()`)  
  Stage called after the optimizer queue is evaluated. Called in
  `OptimInstance$.eval_queue()`. The functions must have two arguments
  named `callback` and `context`.

- on_worker_end:

  (`function()`)  
  Stage called at the end of the optimization on the worker. Called in
  the worker loop. The functions must have two arguments named
  `callback` and `context`.

- on_tuning_result_begin:

  (`function()`)  
  Stage called at the beginning of the result writing. Called in
  `TuningInstance*$assign_result()`. The functions must have two
  arguments named `callback` and `context`. The arguments of
  `$assign_result(xdt, y, learner_param_vals, extra)` are available in
  `context`.

- on_result_begin:

  (`function()`)  
  Stage called at the beginning of the result writing. Called in
  `OptimInstance$assign_result()`. The functions must have two arguments
  named `callback` and `context`. The arguments of
  `$.assign_result(xdt, y, extra)` are available in the `context`.

- on_result_end:

  (`function()`)  
  Stage called after the result is written. Called in
  `OptimInstance$assign_result()`. The functions must have two arguments
  named `callback` and `context`. The final result `instance$result` is
  available in the `context`.

- on_result:

  (`function()`)  
  Deprecated. Use `on_result_end` instead. Stage called after the result
  is written. Called in `OptimInstance$assign_result()`.

- on_optimization_end:

  (`function()`)  
  Stage called at the end of the optimization. Called in
  `Optimizer$optimize()`.

## Details

When implementing a callback, each function must have two arguments
named `callback` and `context`. A callback can write data to the state
(`$state`), e.g. settings that affect the callback itself. Tuning
callbacks access
[ContextAsyncTuning](https://mlr3tuning.mlr-org.com/reference/ContextAsyncTuning.md)
and
[mlr3::ContextResample](https://mlr3.mlr-org.com/reference/ContextResample.html).
