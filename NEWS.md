# mlr3tuning 0.9.0

- Adds `AutoTuner$base_learner()` method to extract the base learner from 
  nested learner objects.
- `tune()` supports multi-criteria tuning.
- Allows empty search space.
- Adds `TunerIrace` from `irace` package.
- `extract_inner_tuning_archives()` helper function to extract inner tuning
  archives.
- Removes `ArchiveTuning$extended_archive()` method. The `mlr3::ResampleResults` are
  joined automatically by `as.data.table.TuningArchive()` and
  `extract_inner_tuning_archives()`.

# mlr3tuning 0.8.0

- Adds `tune()`, `auto_tuner()` and `tune_nested()` sugar functions.
- `TuningInstanceSingleCrit`, `TuningInstanceMultiCrit` and `AutoTuner` can be
  initialized with `store_benchmark_result = FALSE` and `store_models = TRUE`
  to allow measures to access the models. 
- Prettier printing methods.

# mlr3tuning 0.7.0

- Fix `TuningInstance*$assign_result()` errors with required parameter bug.
- Shortcuts to access `$learner()`, `$learners()`, `$learner_param_vals()`,
  `$predictions()` and `$resample_result()` from benchmark result in archive.
- `extract_inner_tuning_results()` helper function to extract inner tuning 
  results.

# mlr3tuning 0.6.0

- `ArchiveTuning$data` is a public field now.

# mlr3tuning 0.5.0

- Adds `TunerCmaes` from `adagio` package.
- Fix `predict_type` in `AutoTuner`.
- Support to set `TuneToken` in `Learner$param_set` and create a search space
  from it.
- The order of the parameters in `TuningInstanceSingleCrit` and
  `TuningInstanceSingleCrit` changed.

# mlr3tuning 0.4.0

- Option to control `store_benchmark_result`, `store_models` and `check_values`
  in `AutoTuner`. `store_tuning_instance` must be set as a parameter during
  initialization.
- Fixes `check_values` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`.
- Removed dependency on orphaned package `bibtex`.

# mlr3tuning 0.3.0

- Compact in-memory representation of R6 objects to save space when
  saving mlr3 objects via saveRDS(), serialize() etc.
- `Archive` is `ArchiveTuning` now which stores the benchmark result in
  `$benchmark_result`. This change removed the resample results from the archive
  but they can be still accessed via the benchmark result.
- Warning message if external package for tuning is not installed.
- To retrieve the inner tuning results in nested resampling,
  `as.data.table(rr)$learner[[1]]$tuning_result` must be used now.

# mlr3tuning 0.2.0

- `TuningInstance` is now `TuningInstanceSingleCrit`. `TuningInstanceMultiCrit`
  is still available for multi-criteria tuning.
- Terminators are now accessible by `trm()` and `trms()` instead of `term()` and
  `terms()`.
- Storing of resample results is optional now by using the
  `store_resample_result` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`
- `TunerNLoptr` adds non-linear optimization from the nloptr package.
- Logging is controlled by the `bbotk` logger now.
- Proposed points and performance values can be checked for validity by
  activating the `check_values` flag in `TuningInstanceSingleCrit` and
  `TuningInstanceMultiCrit`.

# mlr3tuning 0.1.3

- mlr3tuning now depends on the `bbotk` package for basic tuning objects.
  `Terminator` classes now live in `bbotk`. As a consequence `ObjectiveTuning`
  inherits from `bbotk::Objective`, `TuningInstance` from `bbotk::OptimInstance`
  and `Tuner` from `bbotk::Optimizer`
- `TuningInstance$param_set` becomes `TuningInstance$search_space` to avoid
  confusion as the `param_set` usually contains the parameters that change the
  behaviour of an object.
- Tuning is triggered by `$optimize()` instead of `$tune()`

# mlr3tuning 0.1.2

- Fixed a bug in `AutoTuner` where a `$clone()` was missing. Tuning results are
  unaffected, only stored models contained wrong hyperparameter values (#223).
- Improved output log (#218).

# mlr3tuning 0.1.1

- Maintenance release.

# mlr3tuning 0.1.0

- Initial prototype.
