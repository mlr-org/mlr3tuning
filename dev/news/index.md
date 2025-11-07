# Changelog

## mlr3tuning (development version)

## mlr3tuning 1.4.0

CRAN release: 2025-06-04

- feat: Resample stages from `CallbackResample` are now available in
  `CallbackBatchTuning` and `CallbackAsyncTuning`.
- fix: The `$predict_type` was written to the model even when the
  `AutoTuner` was not trained.
- feat: Internal tuned values are now visible in logs.
- BREAKING CHANGE: Remove internal search space argument.
- BREAKING CHANGE: The mlr3 ecosystem has a base logger now which is
  named `mlr3`. The `mlr3/bbotk` logger is a child of the `mlr3` logger
  and is used for logging messages from the `bbotk` and `mlr3tuning`
  package.
- feat: Classes are now printed with the `cli` package.

## mlr3tuning 1.3.0

CRAN release: 2024-12-17

- feat: Save `ArchiveAsyncTuning` to a `data.table` with
  `ArchiveAsyncTuningFrozen`.
- perf: Save models on worker only when requested in
  `ObjectiveTuningAsync`.

## mlr3tuning 1.2.1

CRAN release: 2024-11-26

- refactor: Only pass `extra` to `$assign_result()`.

## mlr3tuning 1.2.0

CRAN release: 2024-11-08

- feat: Add new callback `clbk("mlr3tuning.one_se_rule")` that selects
  the the hyperparameter configuration with the smallest feature set
  within one standard error of the best.
- feat: Add new stages `on_tuning_result_begin` and `on_result_begin` to
  `CallbackAsyncTuning` and `CallbackBatchTuning`.
- refactor: Rename stage `on_result` to `on_result_end` in
  `CallbackAsyncTuning` and `CallbackBatchTuning`.
- docs: Extend the `CallbackAsyncTuning` and `CallbackBatchTuning`
  documentation.
- compatibility: mlr3 0.22.0
- compatibility: Work with new irace 4.0.0

## mlr3tuning 1.1.0

CRAN release: 2024-10-27

- fix: The `as_data_table()` functions do not unnest the `x_domain`
  colum anymore by default.
- fix: `to_tune(internal = TRUE)` now also works if non-internal tuning
  parameters require have an `.extra_trafo`.
- feat: It is now possible to pass an `internal_search_space` manually.
  This allows to use parameter transformations on the primary search
  space in combination with internal hyperparameter tuning.
- refactor: The `Tuner` pass extra information of the result in the
  `extra` parameter now.

## mlr3tuning 1.0.2

CRAN release: 2024-10-14

- refactor: Extract internal tuned values in instance.

## mlr3tuning 1.0.1

CRAN release: 2024-09-10

- refactor: Replace internal tuning callback.
- perf: Delete intermediate `BenchmarkResult` in `ObjectiveTuningBatch`
  after optimization.

## mlr3tuning 1.0.0

CRAN release: 2024-06-29

- feat: Introduce asynchronous optimization with the `TunerAsync` and
  `TuningInstanceAsync*` classes.
- BREAKING CHANGE: The `Tuner` class is `TunerBatch` now.
- BREAKING CHANGE: THe `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit` classes are `TuningInstanceBatchSingleCrit`
  and `TuningInstanceBatchMultiCrit` now.
- BREAKING CHANGE: The `CallbackTuning` class is `CallbackBatchTuning`
  now.
- BREAKING CHANGE: The `ContextEval` class is `ContextBatchTuning` now.
- refactor: Remove hotstarting from batch optimization due to low
  performance.
- refactor: The option `evaluate_default` is a callback now.

## mlr3tuning 0.20.0

CRAN release: 2024-03-05

- compatibility: Work with new paradox version 1.0.0
- fix: `TunerIrace` failed with logical parameters and dependencies.
- Added marshaling support to `AutoTuner`

## mlr3tuning 0.19.2

CRAN release: 2023-11-28

- refactor: Change thread limits.

## mlr3tuning 0.19.1

CRAN release: 2023-11-20

- refactor: Speed up the tuning process by minimizing the number of deep
  clones and parameter checks.
- fix: Set `store_benchmark_result = TRUE` if `store_models = TRUE` when
  creating a tuning instance.
- fix: Passing a terminator in
  [`tune_nested()`](https://mlr3tuning.mlr-org.com/dev/reference/tune_nested.md)
  did not work.

## mlr3tuning 0.19.0

CRAN release: 2023-06-26

- fix: Add `$phash()` method to `AutoTuner`.
- fix: Include `Tuner` in hash of `AutoTuner`.
- feat: Add new callback that scores the configurations on additional
  measures while tuning.
- feat: Add vignette about adding new tuners which was previously part
  of the mlr3book.

## mlr3tuning 0.18.0

CRAN release: 2023-03-08

- BREAKING CHANGE: The `method` parameter of
  [`tune()`](https://mlr3tuning.mlr-org.com/dev/reference/tune.md),
  [`tune_nested()`](https://mlr3tuning.mlr-org.com/dev/reference/tune_nested.md)
  and
  [`auto_tuner()`](https://mlr3tuning.mlr-org.com/dev/reference/auto_tuner.md)
  is renamed to `tuner`. Only `Tuner` objects are accepted now.
  Arguments to the tuner cannot be passed with `...` anymore.
- BREAKING CHANGE: The `tuner` parameter of `AutoTuner` is moved to the
  first position to achieve consistency with the other functions.
- docs: Update resources sections.
- docs: Add list of default measures.
- fix: Add `allow_hotstarting`, `keep_hotstart_stack` and `keep_models`
  flags to `AutoTuner` and
  [`auto_tuner()`](https://mlr3tuning.mlr-org.com/dev/reference/auto_tuner.md).

## mlr3tuning 0.17.2

CRAN release: 2022-12-22

- feat: `AutoTuner` accepts instantiated resamplings now. The
  `AutoTuner` checks if all row ids of the inner resampling are present
  in the outer resampling train set when nested resampling is performed.
- fix: Standalone `Tuner` did not create a `ContextOptimization`.

## mlr3tuning 0.17.1

CRAN release: 2022-12-07

- fix: The [`ti()`](https://mlr3tuning.mlr-org.com/dev/reference/ti.md)
  function did not accept callbacks.

## mlr3tuning 0.17.0

CRAN release: 2022-11-18

- feat: The methods `$importance()`, `$selected_features()`,
  `$oob_error()` and `$loglik()` are forwarded from the final model to
  the `AutoTuner` now.
- refactor: The `AutoTuner` stores the instance and benchmark result if
  `store_models = TRUE`.
- refactor: The `AutoTuner` stores the instance if
  `store_benchmark_result = TRUE`.

## mlr3tuning 0.16.0

CRAN release: 2022-11-08

- feat: Add new callback that enables early stopping while tuning to
  `mlr_callbacks`.
- feat: Add new callback that backups the benchmark result to disk after
  each batch.
- feat: Create custom callbacks with the
  [`callback_batch_tuning()`](https://mlr3tuning.mlr-org.com/dev/reference/callback_batch_tuning.md)
  function.

## mlr3tuning 0.15.0

CRAN release: 2022-10-21

- fix: `AutoTuner` did not accept `TuningSpace` objects as search
  spaces.
- feat: Add [`ti()`](https://mlr3tuning.mlr-org.com/dev/reference/ti.md)
  function to create a `TuningInstanceSingleCrit` or
  `TuningInstanceMultiCrit`.
- docs: Documentation has a technical details section now.
- feat: New option for
  [`extract_inner_tuning_results()`](https://mlr3tuning.mlr-org.com/dev/reference/extract_inner_tuning_results.md)
  to return the tuning instances.

## mlr3tuning 0.14.0

CRAN release: 2022-08-25

- feat: Add option `evaluate_default` to evaluate learners with
  hyperparameters set to their default values.
- refactor: From now on, the default of `smooth` is `FALSE` for
  `TunerGenSA`.

## mlr3tuning 0.13.1

CRAN release: 2022-05-03

- feat: `Tuner` objects have the field `$id` now.

## mlr3tuning 0.13.0

CRAN release: 2022-04-06

- feat: Allow to pass `Tuner` objects as `method` in
  [`tune()`](https://mlr3tuning.mlr-org.com/dev/reference/tune.md) and
  [`auto_tuner()`](https://mlr3tuning.mlr-org.com/dev/reference/auto_tuner.md).
- docs: Link `Tuner` to help page of
  [`bbotk::Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html).
- feat: `Tuner` objects have the optional field `$label` now.
- feat:
  [`as.data.table()`](https://rdatatable.gitlab.io/data.table/reference/as.data.table.html)
  functions for objects of class `Dictionary` have been extended with
  additional columns.

## mlr3tuning 0.12.1

CRAN release: 2022-02-25

- feat: Add a `as.data.table.DictionaryTuner` function.
- feat: New `$help()` method which opens the manual page of a `Tuner`.

## mlr3tuning 0.12.0

CRAN release: 2022-02-17

- feat:
  [`as_search_space()`](https://mlr3tuning.mlr-org.com/dev/reference/as_search_space.md)
  function to create search spaces from `Learner` and `ParamSet`
  objects. Allow to pass `TuningSpace` objects as `search_space` in
  `TuningInstanceSingleCrit` and `TuningInstanceMultiCrit`.
- feat: The
  [`mlr3::HotstartStack`](https://mlr3.mlr-org.com/reference/HotstartStack.html)
  can now be removed after tuning with the `keep_hotstart_stack` flag.
- feat: The `Archive` stores errors and warnings of the learners.
- feat: When no measure is provided, the default measure is used in
  [`auto_tuner()`](https://mlr3tuning.mlr-org.com/dev/reference/auto_tuner.md)
  and
  [`tune_nested()`](https://mlr3tuning.mlr-org.com/dev/reference/tune_nested.md).

## mlr3tuning 0.11.0

CRAN release: 2022-02-02

- fix: `$assign_result()` method in `TuningInstanceSingleCrit` when
  search space is empty.
- feat: Default measure is used when no measure is supplied to
  `TuningInstanceSingleCrit`.

## mlr3tuning 0.10.0

CRAN release: 2022-01-20

- Fixes bug in `TuningInstanceMultiCrit$assign_result()`.
- Hotstarting of learners with previously fitted models.
- Remove deep clones to speed up tuning.
- Add `store_models` flag to
  [`auto_tuner()`](https://mlr3tuning.mlr-org.com/dev/reference/auto_tuner.md).
- Add `"noisy"` property to `ObjectiveTuning`.

## mlr3tuning 0.9.0

CRAN release: 2021-09-14

- Adds `AutoTuner$base_learner()` method to extract the base learner
  from nested learner objects.
- [`tune()`](https://mlr3tuning.mlr-org.com/dev/reference/tune.md)
  supports multi-criteria tuning.
- Allows empty search space.
- Adds `TunerIrace` from `irace` package.
- [`extract_inner_tuning_archives()`](https://mlr3tuning.mlr-org.com/dev/reference/extract_inner_tuning_archives.md)
  helper function to extract inner tuning archives.
- Removes `ArchiveTuning$extended_archive()` method. The
  `mlr3::ResampleResults` are joined automatically by
  `as.data.table.TuningArchive()` and
  [`extract_inner_tuning_archives()`](https://mlr3tuning.mlr-org.com/dev/reference/extract_inner_tuning_archives.md).

## mlr3tuning 0.8.0

CRAN release: 2021-03-12

- Adds [`tune()`](https://mlr3tuning.mlr-org.com/dev/reference/tune.md),
  [`auto_tuner()`](https://mlr3tuning.mlr-org.com/dev/reference/auto_tuner.md)
  and
  [`tune_nested()`](https://mlr3tuning.mlr-org.com/dev/reference/tune_nested.md)
  sugar functions.
- `TuningInstanceSingleCrit`, `TuningInstanceMultiCrit` and `AutoTuner`
  can be initialized with `store_benchmark_result = FALSE` and
  `store_models = TRUE` to allow measures to access the models.
- Prettier printing methods.

## mlr3tuning 0.7.0

CRAN release: 2021-02-11

- Fix `TuningInstance*$assign_result()` errors with required parameter
  bug.
- Shortcuts to access `$learner()`, `$learners()`,
  `$learner_param_vals()`, `$predictions()` and `$resample_result()`
  from benchmark result in archive.
- [`extract_inner_tuning_results()`](https://mlr3tuning.mlr-org.com/dev/reference/extract_inner_tuning_results.md)
  helper function to extract inner tuning results.

## mlr3tuning 0.6.0

CRAN release: 2021-01-24

- `ArchiveTuning$data` is a public field now.

## mlr3tuning 0.5.0

CRAN release: 2020-12-07

- Adds `TunerCmaes` from `adagio` package.
- Fix `predict_type` in `AutoTuner`.
- Support to set `TuneToken` in `Learner$param_set` and create a search
  space from it.
- The order of the parameters in `TuningInstanceSingleCrit` and
  `TuningInstanceSingleCrit` changed.

## mlr3tuning 0.4.0

CRAN release: 2020-10-07

- Option to control `store_benchmark_result`, `store_models` and
  `check_values` in `AutoTuner`. `store_tuning_instance` must be set as
  a parameter during initialization.
- Fixes `check_values` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`.
- Removed dependency on orphaned package `bibtex`.

## mlr3tuning 0.3.0

CRAN release: 2020-09-08

- Compact in-memory representation of R6 objects to save space when
  saving mlr3 objects via
  [`saveRDS()`](https://rdrr.io/r/base/readRDS.html),
  [`serialize()`](https://rdrr.io/r/base/serialize.html) etc.
- `Archive` is `ArchiveTuning` now which stores the benchmark result in
  `$benchmark_result`. This change removed the resample results from the
  archive but they can be still accessed via the benchmark result.
- Warning message if external package for tuning is not installed.
- To retrieve the inner tuning results in nested resampling,
  `as.data.table(rr)$learner[[1]]$tuning_result` must be used now.

## mlr3tuning 0.2.0

CRAN release: 2020-07-28

- `TuningInstance` is now `TuningInstanceSingleCrit`.
  `TuningInstanceMultiCrit` is still available for multi-criteria
  tuning.
- Terminators are now accessible by
  [`trm()`](https://bbotk.mlr-org.com/reference/trm.html) and
  [`trms()`](https://bbotk.mlr-org.com/reference/trm.html) instead of
  `term()` and [`terms()`](https://rdrr.io/r/stats/terms.html).
- Storing of resample results is optional now by using the
  `store_resample_result` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`
- `TunerNLoptr` adds non-linear optimization from the nloptr package.
- Logging is controlled by the `bbotk` logger now.
- Proposed points and performance values can be checked for validity by
  activating the `check_values` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`.

## mlr3tuning 0.1.3

- mlr3tuning now depends on the `bbotk` package for basic tuning
  objects. `Terminator` classes now live in `bbotk`. As a consequence
  `ObjectiveTuning` inherits from
  [`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html),
  `TuningInstance` from
  [`bbotk::OptimInstance`](https://bbotk.mlr-org.com/reference/OptimInstance.html)
  and `Tuner` from
  [`bbotk::Optimizer`](https://bbotk.mlr-org.com/reference/Optimizer.html)
- `TuningInstance$param_set` becomes `TuningInstance$search_space` to
  avoid confusion as the `param_set` usually contains the parameters
  that change the behavior of an object.
- Tuning is triggered by `$optimize()` instead of `$tune()`

## mlr3tuning 0.1.2

CRAN release: 2020-01-31

- Fixed a bug in `AutoTuner` where a `$clone()` was missing. Tuning
  results are unaffected, only stored models contained wrong
  hyperparameter values
  ([\#223](https://github.com/mlr-org/mlr3tuning/issues/223)).
- Improved output log
  ([\#218](https://github.com/mlr-org/mlr3tuning/issues/218)).

## mlr3tuning 0.1.1

CRAN release: 2019-12-06

- Maintenance release.

## mlr3tuning 0.1.0

CRAN release: 2019-09-30

- Initial prototype.
