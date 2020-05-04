# mlr3tuning 0.1.3

* mlr3tuning now depends on the `bbotk` package for basic tuning objects.
  `Terminator` classes now live in `bbotk`. As a consequence `ObjectiveTuning`
  inherits from `bbotk::Objective`, `TuningInstance` from `bbotk::OptimInstance`
  and `Tuner` from `bbotk::Optimizer`
* `TuningInstance$param_set` becomes `TuningInstance$search_space` to avoid
  confusion as the `param_set` usually contains the parameters that change the
  behaviour of an object.

# mlr3tuning 0.1.2

* Fixed a bug in `AutoTuner` where a `$clone()` was missing. Tuning results are
  unaffected, only stored models contained wrong hyperparameter values (#223).
* Improved output log (#218).

# mlr3tuning 0.1.1

* Maintenance release.

# mlr3tuning 0.1.0

* Initial prototype.
