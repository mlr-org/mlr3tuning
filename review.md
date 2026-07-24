# Critical code review: mlr3tuning (whole package)

Review of all 48 files in `R/` (~6,300 lines), conducted adversarially by subsystem: core tuner/objective classes, tuning instances, archives, AutoTuner and sugar, the callback system, and the eleven concrete tuner wrappers.
Every Blocking finding below was either reproduced live in an R session or traced through the installed bbotk/mlr3 sources; findings that rest on static analysis alone are marked as such.

## Summary

The package core is solid where it is oldest and best-tested: the batch objective's clone/reset discipline, the internal-search-space pipeline, the callback stage wiring, and the irace target runner are all correct and well-covered.
The damage is concentrated at the seams: **seven blocking bugs**, most of them either reference-semantics corruption (data.table `set()` on shared tables, R6 shallow state), silent recycling (`length()` on a data.table, unchecked design recycling), or crashes on the async path that only fire under realistic conditions (leftover queued tasks, `store_benchmark_result = FALSE`, second `optimize()` call).
The single biggest structural problem is **four-way copy-paste drift**: the Batch/Async × SingleCrit/MultiCrit twins were forked, then fixed and documented independently, and they have diverged in validation, defaults, docs, and behavior in at least a dozen places.
The documentation rot is severe — multiple man pages document parameters, defaults, columns, and behavior that do not exist.

## Critical issues (Blocking) Done

1. **`AutoTuner$clone(deep = TRUE)` shares mutable state with the original** — `R/AutoTuner.R:121-127`, no `deep_clone` method exists. `instance_args` is a plain list of R6 objects (learner, resampling, terminator, callbacks); R6's default deep clone does not descend into lists. Reproduced: after `at2 = at$clone(deep = TRUE); at2$predict_type = "prob"`, the *original's* inner learner has `predict_type = "prob"`, because the `predict_type` setter (`AutoTuner.R:340`) writes through the shared reference. mlr3 deep-clones learners routinely (`resample()`, `benchmark_grid()`), so this corrupts sibling learners in ordinary benchmarks. This is the classic AutoTuner clone bug and there are zero clone tests in `test_AutoTuner.R`. Fix: implement `deep_clone` that deep-clones `instance_args` (and the model's learner/instance).

2. **`extract_inner_tuning_results.ResampleResult` corrupts the stored tuning instance by reference** — `R/extract_inner_tuning_results.R:73-79`. `learner$tuning_result` returns the instance's private `.result` data.table *without a copy*; `setalloccol` + `set()` then write the `iteration` and `tuning_instance` columns straight into the stored result. Reproduced: after one call, `rr$learners[[1]]$tuning_instance$result` gains both columns, and the instance's own result table embeds a self-reference to the instance (`identical(inst$result$tuning_instance[[1]], inst)` is `TRUE`). A later call with `tuning_instance = FALSE` leaks the stale column into its output. Fix: `data = copy(learner$tuning_result)`. The sibling `extract_inner_tuning_archives.R:81` gets this right by operating on a fresh `as.data.table()` result.

3. **`TuningInstance{Batch,Async}MultiCrit$assign_result` recycles silently in the empty-search-space branch** — `R/TuningInstanceBatchMulticrit.R:195`, `R/TuningInstanceAsyncMulticrit.R:169`: `opt_x = replicate(length(private$.result_ydt), list())`. `private$.result_ydt` is a data.table, so `length()` counts **columns (measures), not rows (Pareto points)**; the sibling branch six lines up correctly uses `nrow()`. Reachable whenever the primary search space is empty (all-internal tuning, constant configs). Reproduced: with a 3-point front and 2 measures, `Map(insert_named, ...)` recycles with a warning and produces wrong `result_learner_param_vals`. The existing test passes only because 10 front points happen to be divisible by 2 measures. Fix: `nrow(private$.result_ydt)` in both files.

4. **`ArchiveAsyncTuning$benchmark_result` permanently self-corrupts when `store_benchmark_result = FALSE`** — `R/ArchiveAsyncTuning.R:181-188`. With no `resample_result` column (a supported configuration, exercised in `test_ArchiveAsyncTuning.R:239`), `map(NULL, as_benchmark_result)` is `list()` and `Reduce(...)` returns `NULL`, overwriting the cached `BenchmarkResult` with `NULL`. First access returns `NULL`; every later access dies inside `if (n_finished_tasks > NULL)` with "argument is of length zero". The corruption propagates into `ArchiveAsyncTuningFrozen$initialize` and thus the `async_freeze_archive` callback. (Traced statically plus verified `Reduce`/`NULL` semantics; needs rush workers for an end-to-end repro.) Fix: guard on column existence and raise a clear "benchmark result was not stored" error.

5. **`mlr3tuning.one_se_rule` (async) crashes at result assignment on real async archives** — `R/mlr_callbacks.R:389-399`. `as.data.table(archive)` for async archives includes queued/running/failed rows (verified in bbotk's `data_with_state()`), whose scores are `NA`; `se = sd(y) / sqrt(length(y))` is then `NA` (no `na.rm`) and `if (se == 0)` errors with "missing value where TRUE/FALSE needed" — *after* the entire tuning run, destroying the result. Leftover queued/running tasks at termination are the normal async case. A single-row archive crashes the same way in both variants, and the batch twin (`mlr_callbacks.R:447-451`) lacks even the `!nrow(data)` guard. Even when it survives, `length(y)` counts NA rows and deflates the SE.

6. **`TunerBatchIrace$optimize()` destroys its own configuration; a second run is impossible** — `R/TunerBatchIrace.R:116-135`. It deletes `n_instances` from and injects instantiated `Resampling` objects into the *live shared* param set (`private$.optimizer$param_set$values = pv`). Reproduced: after one successful `optimize()`, `n_instances` is `NULL`, ten resamplings sit permanently in `$values$instances` (printed, serialized, hashed), and a second `optimize()` dies with `invalid 'length' argument`. This breaks training the same irace `AutoTuner` twice. Fix: operate on a local copy and restore the param set (e.g. `on.exit`).

7. **`ObjectiveTuningBatch` silently mis-pairs configurations with resamplings via data.table recycling** — `R/ObjectiveTuningBatch.R:67-74`. In the `length(resampling) > 1` branch (the custom-resampling extension point used by callbacks and extension packages), the design pairs `xss[[i]]` with `resampling[[i]]` with no length assertion. Reproduced: 4 configs × 2 resamplings recycles 1,2,1,2 with **no warning**, producing silently wrong archive entries. Fix: assert `length(xss) == length(resampling)`.

## Required changes

### Copy-paste drift between the Batch/Async twins Done

This is the package's systemic disease. The concrete divergences, each a real behavioral difference:

- **Hotstarting**: `ObjectiveTuningAsync.R:40` hardcodes `allow_hotstart = TRUE`; `ObjectiveTuningBatch.R:82-87` never enables it. Meanwhile `TunerBatchGridSearch.R:11-12` promises "the grid is sorted by the hotstart parameter" — verified false: bbotk's grid search always shuffles and the batch objective can't hotstart at all. One side is wrong; the docs are wrong either way.
- **Callback class validation**: `assert_async_tuning_callbacks()` / `assert_batch_tuning_callbacks()` (`CallbackAsyncTuning.R:310-312`, `CallbackBatchTuning.R:288-290`) are byte-identical and only check for class `Callback` — the singular, class-checking variants are unused by the instance constructors. A `CallbackBatchTuning` passed to an async instance is accepted silently and fails confusingly at runtime.
- **`as_measures` coercion**: the batch measures callback coerces and clones (`mlr_callbacks.R:80`); the async twin (`mlr_callbacks.R:108`) does neither, so the documented usage style (`measures = msr(...)`) errors on async. They share one man page.
- **`assert_list` on `learner_param_vals`**: both SingleCrit `assign_result` methods validate it; both MultiCrit twins (`TuningInstanceBatchMulticrit.R:163`, `TuningInstanceAsyncMulticrit.R:137`) don't, despite the stricter contract.
- **`n_features` column**: async stores a *named* numeric from `aggregate()` (`mlr_callbacks.R:380-381`); batch stores the plain column (`mlr_callbacks.R:436-437`).

### Reference semantics and mutable-state hazards

- `as_tuner(x, clone = TRUE)` performs a **shallow** clone (`R/as_tuner.R:21-24`); verified that the "clone" shares its `ParamSet` with the original, so setting a hyperparameter on one mutates both. mlr3's `as_learner` uses `clone(deep = TRUE)` for exactly this reason. There is no `test_as_tuner.R` at all.
- `internal_search_space` is a bare mutable public field in **all four** instance classes (e.g. `TuningInstanceBatchSingleCrit.R:148`), and `ArchiveBatchTuning$benchmark_result` (`ArchiveBatchTuning.R:75`) is likewise a plain public field — both violate the house active-binding rule and both are load-bearing (`assign_result` calls methods on the former; `as.data.table` alignment assumes the latter untouched).
- `Tuner$label` binding guard compares `rhs` against `private$.param_set` instead of `private$.label` (`R/Tuner.R:143-148`) — a copy-paste bug: assigning the tuner's own param set to `$label` is silently accepted; assigning its current label errors. `Tuner$id` (`Tuner.R:34`) is a plain public field with no post-construction validation (`t$id = 123` succeeds).
- The context setters poke `self$instance$objective$.__enclos_env__$private$...` directly, ten times across `ContextAsyncTuning.R` / `ContextBatchTuning.R`, while the getters use `get_private()`; mlr3misc exports `` `get_private<-` `` for this.

### AutoTuner lifecycle

- **`hash` omits `predict_sets`, `validate`, `use_weights`** (`AutoTuner.R:351-364`) relative to base `Learner$hash`. Reproduced: two AutoTuners differing only in `predict_sets` share a hash — benchmark deduplication collides.
- **Marshaling contract violations**: mixed-`inplace` round trips drop the `auto_tuner_model` class (`AutoTuner.R:474-494`; reproduced — subsequent `marshal_model` dispatches to the no-op default), and `marshaled` is a public *method* while the ecosystem convention is a logical active binding (`AutoTuner.R:295-299`) — `if (learner$marshaled)` errors for AutoTuner only.
- **Marshaled state silently degrades accessors** (`AutoTuner.R:309-320`): after `$marshal()`, `$learner` silently returns the *untrained* learner, `$tuning_instance` returns `NULL`, and `importance()` et al. produce misleading errors. No hint that the model is merely marshaled.
- **Instantiated-resampling check is dead for data.table-backed resamplings** (`AutoTuner.R:385-409`): it reads `instance$train`/`$test`, which only exist for custom/holdout; for `cv` etc. `imap(NULL, ...)` iterates zero times and the friendly error never fires. Worse, the documented use case (instantiated inner resampling on a subset, docs lines 37-41) fails outright against current mlr3 dev due to the `task_row_hash` comparison; the only test trains on the same task, masking it.
- **`rush` + batch tuner**: accepted at construction, dies at `$train()` with `unused argument (rush = ...)` (`AutoTuner.R:175-177, 411-416`); in `tune()` (`tune.R:121-138`) a user-supplied `rush` is **silently dropped** on the batch path and never class-checked. Validate the combination up front.
- **Silent store-flag escalation**, twice: `store_benchmark_result || store_models` (`AutoTuner.R:169-171`, same pattern in `ObjectiveTuning.R:69`) silently overrides an explicit `FALSE`, while the shared template `man-roxygen/param_store_models.R` explicitly documents the forbidden combination as supported. Warn or fix the docs.

### one_se_rule statistics

`se = sd(y) / sqrt(length(y))` (`mlr_callbacks.R:396-397, 448-449`) is the SE of mean performance *across archive configurations*, not the resampling SE of the best configuration that the cited Kuhn & Johnson rule requires. As implemented, the tolerance window grows with search-space width and shrinks to zero as evaluations accumulate. The per-fold scores needed for the correct SE are available (the callback already forces `store_models = TRUE`). It also has no multi-crit guard: `data[[archive$cols_y]]` with two measures does recursive `[[` indexing (`mlr_callbacks.R:396, 448`).

### Documentation that contradicts the code

Every item verified against the generated `man/` pages or the installed bbotk:

- `TunerAsyncGridSearch.R:8`: `@templateVar id async_design_points` — the rendered Dictionary section tells users to construct the **wrong tuner** (`tnr("async_design_points")`).
- `TunerAsyncFromOptimizerAsync.R:36-42`: `optimize()` docs reference the **Batch** instance classes three times; the code asserts async.
- `TunerBatchNLoptr.R:13-16`: claims package-default tolerances are active; verified all four are initialized to `-1` (deactivated) in bbotk.
- `TunerBatchGenSA.R:18`: parallelization section promises a `batch_size` that the wrapped optimizer does not have (GenSA proposes one point per batch).
- Archive docs (`ArchiveBatchTuning.R:53,80-81,105`, `ArchiveAsyncTuning.R:22-26,33,57-58`, `ArchiveAsyncTuningFrozen.R:13`): stale `as.data.table.ArchiveTuning` name, wrong `unnest`/`exclude_columns`/`check_values` defaults, a documented `check_values` constructor arg that doesn't exist, documented `timestamp`/`batch_nr` columns that don't exist on the async table, and `uhash` documented as `logical(1)` (~15 times) when it is `character(1)`.
- `mlr_callbacks.R:241,275`: both default_configuration callbacks set `man = "mlr3tuning::mlr3tuning.default_configuration"` — the topic does not exist; `callback$help()` errors.
- `mlr_callbacks.R:153,156`: the mlflow example uses a wrong callback id (`rush_mlflow` vs. registered `async_mlflow`) and a nonexistent tuner (`random_search_v2`); its only test is commented out, so the callback is entirely unexercised.
- `ContextAsyncTuning.R:30-31`: `resample_result` documented as `BenchmarkResult`; it is a `ResampleResult`.
- `TunerBatchInternal.R:28-46`: example creates `tsk("pima")` and then tunes `tsk("iris")`; `tune.R:57` example comment says "Palmer Penguins" over `tsk("pima")`.
- `Tuner.R:53-54, 122`: comment claims a crit-property invariant that is not enforced (verified: constructible with neither `single-crit` nor `multi-crit`), and the roxygen cites `mlr_reflections$tuner_properties`, which does not exist (code checks `bbotk_reflections$optimizer_properties`).
- `TuningInstanceAsyncMulticrit.R:133`: `@param ydt (numeric(1))` for a data.table; `TuningInstanceAsyncSingleCrit.R:19`: stray `instance$.assign_result`; `TunerBatchIrace.R:22`: stale class name `TunerIrace`.

### Silent failures

- `as.data.table(archive, measures = ...)` **silently ignores** the requested measures when no benchmark result is stored, in all three archive variants (`ArchiveBatchTuning.R:218-224`, `ArchiveAsyncTuning.R:216`, `ArchiveAsyncTuningFrozen.R:164`). Reproduced for batch: no column, no warning. At minimum `warning_config()`.
- The `on_result` deprecation shim silently clobbers a simultaneously supplied `on_result_end` (`CallbackAsyncTuning.R:275-279`, `CallbackBatchTuning.R:254-258`).

### Dead code and dead plumbing

- `OptimizerBatchInternal` (`TunerBatchInternal.R:80-100`): unexported, unregistered, referenced nowhere, with a `man` pointer to a bbotk topic that does not exist (`$help()` errors). Delete or upstream.
- `extract_benchmark_result_learners` (`helper.R:13-18`): zero usages anywhere.
- The MultiCrit `assign_result` no-op branch `if (length(...) == 0) list()` (`TuningInstanceBatchMulticrit.R:184-186` and async twin).
- Dead defensive `else "tuner"` branch in both FromOptimizer wrappers (`TunerBatchFromBatchOptimizer.R:25`); `assert_optimizer()` already guarantees `id`.
- Stale `@template param_internal_tuned_values` / `param_internal_search_space` across the four instance files match no actual argument — roxygen silently drops them (verified in the generated Rd).

### House-style violations with teeth

- **Ineffective `#nolint next` directives**: written without the space (`#nolint`) at `TuningInstanceBatchSingleCrit.R:45,47,49,59`, `helper.R:13`, `mlr_callbacks.R:236,270,364` — lintr's exclusion regex does not match them, and the lines they "protect" are up to 177 characters. `TunerBatchIrace.R:140` has a bare `# nolint` on its own line, which suppresses nothing. The house rule itself spells the correct form.
- **`@include` collation headers missing or wrong** across the tuner wrappers (seven files with none; four declaring `Tuner.R` instead of the actual parent) and the instance shims. `TunerBatchCmaes.R` currently collates *before* its parent's file; this works only because R6 resolves `inherit` lazily.
- **Missing checkmate assertions on exported functions**: `extract_inner_tuning_results` never asserts `tuning_instance`; `extract_inner_tuning_archives` never asserts `unnest`/`exclude_columns`; `as_tuner` has no `default` method and no `assert_flag(clone)`.

## Suggestions

- **`.onLoad` stomps shared state** (`zzz.R:16`): `x$tuner_properties = "dependencies"` overwrites instead of unioning — any extension package that appended a property first loses it. No `.onUnload` cleanup for the ten registered callbacks.
- **Batch archive measures path is one divergence from wrong**: positional `cbind` of `aggregate()` scores and an inner-join `merge` that would silently drop unmatched rows (`ArchiveBatchTuning.R:223-231`). Commit 13cf7160 fixed exactly this class of bug for async/frozen; port the by-`uhash` join to batch.
- **Async performance**: `benchmark_result` rebuilds all n resample results from Redis on every access after any new task (O(n²) over a run, `ArchiveAsyncTuning.R:183-186`); `as.data.table` triggers two extra full-archive Redis fetches just for column names, on a *different snapshot* than the main table (`ArchiveAsyncTuning.R:233,240`).
- **`ArchiveBatchTuning$print()` prints the full archive twice** (`ArchiveBatchTuning.R:167-185`); async and frozen print once.
- **backup callback** (`mlr_callbacks.R:29-41`): errors on an existing file at start yet overwrites freely during the run; redundant `file.exists`/`unlink` before `saveRDS`; O(n²) re-serialization of the cumulative benchmark result; silently saves an empty result when `store_benchmark_result = FALSE`; defaults to writing `bmr.rds` into the working directory.
- **Objective cleanup is duplicated and not error-safe** (`TunerBatch.R:85-93`, `TunerBatchFromBatchOptimizer.R:44-58`): the `.__enclos_env__` field-clearing runs only on the success path (memory retention after a failed run — exactly when users retry in-session) and `TunerBatchIrace$optimize()` skips it entirely, along with the internal-search-space guard. Extract one helper, wrap in `on.exit`, and hoist the guard into base `TunerBatch$optimize` so third-party subclasses (mlr3hyperband pattern) get it too.
- **`as_search_space.ParamSet` errors on unset required params** (`as_search_space.R:29`): the instance constructors do the identical token probe with `check_required = FALSE`; this one doesn't.
- **`tune_nested()` hardcodes `store_models = TRUE`** on the outer resample (`tune_nested.R:81`) — no memory-lean nested resampling is possible through this function; it also fails to expose `rush`/`id` unlike `auto_tuner()`.
- **Scalar `|` in `if`** at `TuningInstanceBatchSingleCrit.R:279` and `TuningInstanceAsyncSingleCrit.R:191`; `replicate()` without `simplify = FALSE` at `TunerBatchIrace.R:122-125` (works by accident of `simplify2array` semantics).
- **Async tuner docs are skeletal**: three exported user-facing classes with no examples and none of the Resources/Logging/optimizer sections their batch counterparts have.
- **Test gaps found while verifying**: no clone tests for AutoTuner, no `test_as_tuner.R`, no tests for the deprecated instance shims, no test for the batch `mlr3tuning.measures` callback, mlflow callback test fully commented out, and `test_TunerInternal.R` doesn't follow the `test_{name}.R` convention.

## Genuinely well done

- The batch objective's hyperparameter hygiene — restoring `default_values` with `.insert = FALSE` and the async wholesale replacement — correctly prevents leakage between evaluations without cloning the learner, and clone semantics are pinned by tests.
- The #530 fix (per-row NA handling for points without resample results in async/frozen `as.data.table`) is correct, computes scores on the already-fetched snapshot, and has regression tests in both files.
- The callback stage diagrams match the actual `call_back()` sites exactly — all ten stages verified across both objectives and all four instances — and stage-level test coverage is thorough, including the four worker-side `on_resample_*` stages.
- `target_runner_tuning` for irace is correct where it matters: signed scalar cost, the `ParamLgl` repair, and per-experiment resampling threading are all right.
- The suspicious-looking list-column `set()` wrapping in the SingleCrit `assign_result` is correct and is precisely what makes empty search spaces work; the internal-search-space pipeline is byte-for-byte consistent across all four constructors.
- `AutoTuner`'s marshal `on.exit` model restoration avoids deep-cloning potentially huge models, and the validation-lifecycle handling before the final fit (`setdiff(properties, ...)` + `set_validate(NULL)`) is clean.

## Not verified

- Async paths end-to-end under live rush/Redis workers (findings on those paths are static traces plus verified R semantics).
- Whether `BenchmarkResult$aggregate()` guarantees insertion order (underpins the batch archive's positional `cbind`).
- Determinism of hashing whole R6 objects via `calculate_hash` across sessions.
- The `task_row_hash` finding was verified against mlr3 dev (1.7.1.9000); CRAN mlr3 may behave differently.

## Verdict

**Request changes.**
The seven blocking items are real, reproduced (or fully traced) bugs: two corrupt user-visible objects by reference, two crash completed runs at the moment of result assignment, one silently mis-pairs configurations with resamplings, one makes a tuner single-use, and one destroys an archive accessor permanently.
Items 1–3 and 6–7 are small, surgical fixes (`copy()`, `nrow()`, a length assertion, param-set restore, `deep_clone`); 4 and 5 need a guard plus a decision about correct behavior.
The copy-paste drift between the Batch/Async twins is the root cause of roughly half the findings — any fix applied to one twin should be diffed against the other three as a matter of policy.
