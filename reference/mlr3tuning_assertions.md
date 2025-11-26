# Assertion for mlr3tuning objects

Most assertion functions ensure the right class attribute, and
optionally additional properties.

## Usage

``` r
assert_tuner(tuner)

assert_tuners(tuners)

assert_tuner_async(tuner)

assert_tuner_batch(tuner)

assert_tuning_instance(inst)

assert_tuning_instance_async(inst)

assert_tuning_instance_batch(inst)
```

## Arguments

- tuner:

  ([TunerBatch](https://mlr3tuning.mlr-org.com/reference/TunerBatch.md)).

- tuners:

  (list of [Tuner](https://mlr3tuning.mlr-org.com/reference/Tuner.md)).

- inst:

  ([TuningInstanceBatchSingleCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchSingleCrit.md)
  \|
  [TuningInstanceBatchMultiCrit](https://mlr3tuning.mlr-org.com/reference/TuningInstanceBatchMultiCrit.md)).
