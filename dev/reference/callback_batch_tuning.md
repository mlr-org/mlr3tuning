# Create Batch Tuning Callback

Function to create a
[CallbackBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/CallbackBatchTuning.md).
Predefined callbacks are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_callbacks](https://mlr3misc.mlr-org.com/reference/mlr_callbacks.html)
and can be retrieved with
[`clbk()`](https://mlr3misc.mlr-org.com/reference/clbk.html).

Tuning callbacks can be called from different stages of the tuning
process. The stages are prefixed with `on_*`.

    Start Tuning
         - on_optimization_begin
        Start Tuner Batch
             - on_optimizer_before_eval
            Start Evaluation
                 - on_eval_after_design
                     Start Resampling Iteration
                       - on_resample_begin
                       - on_resample_before_train
                       - on_resample_before_predict
                       - on_resample_end
                     End Resampling Iteration
                 - on_eval_after_benchmark
                 - on_eval_before_archive
            End Evaluation
             - on_optimizer_after_eval
        End Tuner Batch
         - on_tuning_result_begin
         - on_result_begin
         - on_result_end
         - on_optimization_end
    End Tuning

See also the section on parameters for more information on the stages. A
tuning callback works with
[ContextBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/ContextBatchTuning.md)
and
[mlr3::ContextResample](https://mlr3.mlr-org.com/reference/ContextResample.html).

## Usage

``` r
callback_batch_tuning(
  id,
  label = NA_character_,
  man = NA_character_,
  on_optimization_begin = NULL,
  on_optimizer_before_eval = NULL,
  on_eval_after_design = NULL,
  on_resample_begin = NULL,
  on_resample_before_train = NULL,
  on_resample_before_predict = NULL,
  on_resample_end = NULL,
  on_eval_after_benchmark = NULL,
  on_eval_before_archive = NULL,
  on_optimizer_after_eval = NULL,
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

- on_optimization_begin:

  (`function()`)  
  Stage called at the beginning of the optimization. Called in
  `Optimizer$optimize()`. The functions must have two arguments named
  `callback` and `context`.

- on_optimizer_before_eval:

  (`function()`)  
  Stage called after the optimizer proposes points. Called in
  `OptimInstance$eval_batch()`. The functions must have two arguments
  named `callback` and `context`. The argument of `$eval_batch(xdt)` is
  available in `context`.

- on_eval_after_design:

  (`function()`)  
  Stage called after the design is created. Called in
  `ObjectiveTuningBatch$eval_many()`. The functions must have two
  arguments named `callback` and `context`. The arguments of
  `$eval_many(xss, resampling)` are available in `context`.
  Additionally, the `design` is available in `context`.

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

- on_eval_after_benchmark:

  (`function()`)  
  Stage called after hyperparameter configurations are evaluated. Called
  in `ObjectiveTuningBatch$eval_many()`. The functions must have two
  arguments named `callback` and `context`. The `benchmark_result` is
  available in `context`.

- on_eval_before_archive:

  (`function()`)  
  Stage called before performance values are written to the archive.
  Called in `ObjectiveTuningBatch$eval_many()`. The functions must have
  two arguments named `callback` and `context`. The
  `aggregated_performance` is available in `context`.

- on_optimizer_after_eval:

  (`function()`)  
  Stage called after points are evaluated. Called in
  `OptimInstance$eval_batch()`. The functions must have two arguments
  named `callback` and `context`. The new configurations and
  performances in `instance$archive` are available in `context`.

- on_tuning_result_begin:

  (`function()`)  
  Stage called at the beginning of the result writing. Called in
  `TuningInstanceBatch$assign_result()`. The functions must have two
  arguments named `callback` and `context`. The arguments of
  `$assign_result(xdt, y, learner_param_vals, extra)` are available in
  `context`.

- on_result_begin:

  (`function()`)  
  Stage called at the beginning of the result writing. Called in
  `OptimInstance$assign_result()`. The functions must have two arguments
  named `callback` and `context`. The arguments of
  `$assign_result(xdt, y, extra)` are available in `context`.

- on_result_end:

  (`function()`)  
  Stage called after the result is written. Called in
  `OptimInstance$assign_result()`. The functions must have two arguments
  named `callback` and `context`. The final result `instance$result` is
  available in `context`.

- on_result:

  (`function()`)  
  Deprecated. Use `on_result_end` instead. Stage called after the result
  is written. Called in `OptimInstance$assign_result()`. The functions
  must have two arguments named `callback` and `context`.

- on_optimization_end:

  (`function()`)  
  Stage called at the end of the optimization. Called in
  `Optimizer$optimize()`. The functions must have two arguments named
  `callback` and `context`.

## Details

When implementing a callback, each function must have two arguments
named `callback` and `context`. A callback can write data to the state
(`$state`), e.g. settings that affect the callback itself. Tuning
callbacks access
[ContextBatchTuning](https://mlr3tuning.mlr-org.com/dev/reference/ContextBatchTuning.md).

## Examples

``` r
# write archive to disk
callback_batch_tuning("mlr3tuning.backup",
  on_optimization_end = function(callback, context) {
    saveRDS(context$instance$archive, "archive.rds")
  }
)
#> <CallbackBatchTuning:mlr3tuning.backup>
#> * Active Stages: on_optimization_end
```
