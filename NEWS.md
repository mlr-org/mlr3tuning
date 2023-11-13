# mlr3tuning 0.19.1

* refactor: Speed up the tuning process by minimizing the number of deep clones and parameter checks.
* fix: Set `store_benchmark_result = TRUE` if `store_models = TRUE` when creating a tuning instance.
* fix: Passing a terminator in `tune_nested()` did not work.

# mlr3tuning 0.19.0

* fix: Add `$phash()` method to `AutoTuner`.
* fix: Include `Tuner` in hash of  `AutoTuner`.
* feat: Add new callback that scores the configurations on additional measures while tuning.
* feat: Add vignette about adding new tuners which was previously part of the mlr3book.

# mlr3tuning 0.18.0

* BREAKING CHANGE: The `method` parameter of `tune()`, `tune_nested()` and `auto_tuner()` is renamed to `tuner`.
  Only `Tuner` objects are accepted now.
  Arguments to the tuner cannot be passed with `...` anymore.
* BREAKING CHANGE: The `tuner` parameter of `AutoTuner` is moved to the first position to achieve consistency with the other functions.
* docs: Update resources sections.
* docs: Add list of default measures.
* fix: Add `allow_hotstarting`, `keep_hotstart_stack` and `keep_models` flags to `AutoTuner` and `auto_tuner()`.

# mlr3tuning 0.17.2

* feat: `AutoTuner` accepts instantiated resamplings now.
  The `AutoTuner` checks if all row ids of the inner resampling are present in the outer resampling train set when nested resampling is performed.
* fix: Standalone `Tuner` did not create a `ContextOptimization`.

# mlr3tuning 0.17.1

* fix: The `ti()` function did not accept callbacks.

# mlr3tuning 0.17.0

* feat: The methods `$importance()`, `$selected_features()`, `$oob_error()` and `$loglik()` are forwarded from the final model to the `AutoTuner` now.
* refactor: The `AutoTuner` stores the instance and benchmark result if `store_models = TRUE`.
* refactor: The `AutoTuner` stores the instance if `store_benchmark_result = TRUE`.

# mlr3tuning 0.16.0

* feat: Add new callback that enables early stopping while tuning to `mlr_callbacks`.
* feat: Add new callback that backups the benchmark result to disk after each batch.
* feat: Create custom callbacks with the `callback_tuning()` function.

# mlr3tuning 0.15.0

* fix: `AutoTuner` did not accept `TuningSpace` objects as search spaces.
* feat: Add `ti()` function to create a `TuningInstanceSingleCrit` or `TuningInstanceMultiCrit`.
* docs: Documentation has a technical details section now.
* feat: New option for `extract_inner_tuning_results()` to return the tuning instances.

# mlr3tuning 0.14.0

* feat: Add option `evaluate_default` to evaluate learners with hyperparameters set to their default values.
* refactor: From now on, the default of `smooth` is `FALSE` for `TunerGenSA`.

# mlr3tuning 0.13.1

* feat: `Tuner` objects have the field `$id` now.

# mlr3tuning 0.13.0

* feat: Allow to pass `Tuner` objects as `method` in `tune()` and `auto_tuner()`.
* docs: Link `Tuner` to help page of `bbotk::Optimizer`.
* feat: `Tuner` objects have the optional field `$label` now.
* feat: `as.data.table()` functions for objects of class `Dictionary` have been extended with additional columns.

# mlr3tuning 0.12.1

* feat: Add a `as.data.table.DictionaryTuner` function.
* feat: New `$help()` method which opens the manual page of a `Tuner`.

# mlr3tuning 0.12.0

* feat: `as_search_space()` function to create search spaces from `Learner` and `ParamSet` objects.
  Allow to pass `TuningSpace` objects as `search_space` in `TuningInstanceSingleCrit` and `TuningInstanceMultiCrit`.
* feat: The `mlr3::HotstartStack` can now be removed after tuning with the `keep_hotstart_stack` flag.
* feat: The `Archive` stores errors and warnings of the learners.
* feat: When no measure is provided, the default measure is used in `auto_tuner()` and `tune_nested()`.

# mlr3tuning 0.11.0

* fix: `$assign_result()` method in `TuningInstanceSingleCrit` when search space is empty.
* feat: Default measure is used when no measure is supplied to `TuningInstanceSingleCrit`.

# mlr3tuning 0.10.0

* Fixes bug in `TuningInstanceMultiCrit$assign_result()`.
* Hotstarting of learners with previously fitted models.
* Remove deep clones to speed up tuning.
* Add `store_models` flag to `auto_tuner()`.
* Add `"noisy"` property to `ObjectiveTuning`.

# mlr3tuning 0.9.0

* Adds `AutoTuner$base_learner()` method to extract the base learner from
  nested learner objects.
* `tune()` supports multi-criteria tuning.
* Allows empty search space.
* Adds `TunerIrace` from `irace` package.
* `extract_inner_tuning_archives()` helper function to extract inner tuning
  archives.
* Removes `ArchiveTuning$extended_archive()` method. The `mlr3::ResampleResults`
  are joined automatically by `as.data.table.TuningArchive()` and
  `extract_inner_tuning_archives()`.

# mlr3tuning 0.8.0

* Adds `tune()`, `auto_tuner()` and `tune_nested()` sugar functions.
* `TuningInstanceSingleCrit`, `TuningInstanceMultiCrit` and `AutoTuner` can be
  initialized with `store_benchmark_result = FALSE` and `store_models = TRUE`
  to allow measures to access the models.
* Prettier printing methods.

# mlr3tuning 0.7.0

* Fix `TuningInstance*$assign_result()` errors with required parameter bug.
* Shortcuts to access `$learner()`, `$learners()`, `$learner_param_vals()`,
  `$predictions()` and `$resample_result()` from benchmark result in archive.
* `extract_inner_tuning_results()` helper function to extract inner tuning
  results.

# mlr3tuning 0.6.0

* `ArchiveTuning$data` is a public field now.

# mlr3tuning 0.5.0

* Adds `TunerCmaes` from `adagio` package.
* Fix `predict_type` in `AutoTuner`.
* Support to set `TuneToken` in `Learner$param_set` and create a search space
  from it.
* The order of the parameters in `TuningInstanceSingleCrit` and
  `TuningInstanceSingleCrit` changed.

# mlr3tuning 0.4.0

* Option to control `store_benchmark_result`, `store_models` and `check_values`
  in `AutoTuner`. `store_tuning_instance` must be set as a parameter during
  initialization.
* Fixes `check_values` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`.
* Removed dependency on orphaned package `bibtex`.

# mlr3tuning 0.3.0

* Compact in-memory representation of R6 objects to save space when
  saving mlr3 objects via `saveRDS()`, `serialize()` etc.
* `Archive` is `ArchiveTuning` now which stores the benchmark result in
  `$benchmark_result`. This change removed the resample results from the archive
  but they can be still accessed via the benchmark result.
* Warning message if external package for tuning is not installed.
* To retrieve the inner tuning results in nested resampling,
  `as.data.table(rr)$learner[[1]]$tuning_result` must be used now.

# mlr3tuning 0.2.0

* `TuningInstance` is now `TuningInstanceSingleCrit`. `TuningInstanceMultiCrit`
  is still available for multi-criteria tuning.
* Terminators are now accessible by `trm()` and `trms()` instead of `term()` and
  `terms()`.
* Storing of resample results is optional now by using the
  `store_resample_result` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`
* `TunerNLoptr` adds non-linear optimization from the nloptr package.
* Logging is controlled by the `bbotk` logger now.
* Proposed points and performance values can be checked for validity by
  activating the `check_values` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`.

# mlr3tuning 0.1.3

* mlr3tuning now depends on the `bbotk` package for basic tuning objects.
  `Terminator` classes now live in `bbotk`. As a consequence `ObjectiveTuning`
  inherits from `bbotk::Objective`, `TuningInstance` from `bbotk::OptimInstance`
  and `Tuner` from `bbotk::Optimizer`
* `TuningInstance$param_set` becomes `TuningInstance$search_space` to avoid
  confusion as the `param_set` usually contains the parameters that change the
  behavior of an object.
* Tuning is triggered by `$optimize()` instead of `$tune()`

# mlr3tuning 0.1.2

* Fixed a bug in `AutoTuner` where a `$clone()` was missing. Tuning results are
  unaffected, only stored models contained wrong hyperparameter values (#223).
* Improved output log (#218).

# mlr3tuning 0.1.1

* Maintenance release.

# mlr3tuning 0.1.0

* Initial prototype.
