# Class for Tuning Objective

Stores the objective function that estimates the performance of
hyperparameter configurations. This class is usually constructed
internally by the
[TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
or
[TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md).

## Super classes

[`bbotk::Objective`](https://bbotk.mlr-org.com/reference/Objective.html)
-\>
[`mlr3tuning::ObjectiveTuning`](https://mlr3tuning.mlr-org.com/reference/ObjectiveTuning.md)
-\> `ObjectiveTuningAsync`

## Methods

### Public methods

- [`ObjectiveTuningAsync$clone()`](#method-ObjectiveTuningAsync-clone)

Inherited methods

- [`bbotk::Objective$eval()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval)
- [`bbotk::Objective$eval_dt()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval_dt)
- [`bbotk::Objective$eval_many()`](https://bbotk.mlr-org.com/reference/Objective.html#method-eval_many)
- [`bbotk::Objective$format()`](https://bbotk.mlr-org.com/reference/Objective.html#method-format)
- [`bbotk::Objective$help()`](https://bbotk.mlr-org.com/reference/Objective.html#method-help)
- [`bbotk::Objective$print()`](https://bbotk.mlr-org.com/reference/Objective.html#method-print)
- [`mlr3tuning::ObjectiveTuning$initialize()`](https://mlr3tuning.mlr-org.com/reference/ObjectiveTuning.html#method-initialize)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ObjectiveTuningAsync$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
