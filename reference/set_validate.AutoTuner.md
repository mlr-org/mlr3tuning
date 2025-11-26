# Configure Validation for AutoTuner

Configure validation data for the learner that is tuned by the
`AutoTuner`.

## Usage

``` r
# S3 method for class 'AutoTuner'
set_validate(learner, validate, ...)
```

## Arguments

- learner:

  ([`AutoTuner`](https://mlr3tuning.mlr-org.com/reference/AutoTuner.md))  
  The autotuner for which to enable validation.

- validate:

  (`numeric(1)`, `"predefined"`, `"test"`, or `NULL`)  
  How to configure the validation during the hyperparameter tuning.

- ...:

  (any)  
  Passed when calling
  [`set_validate()`](https://mlr3.mlr-org.com/reference/mlr_sugar.html)
  on the wrapped leaerner.

## Examples

``` r
at = auto_tuner(
  tuner = tnr("random_search"),
  learner = lrn("classif.debug", early_stopping = TRUE,
    iter = to_tune(upper = 1000L, internal = TRUE), validate = 0.2),
  resampling = rsmp("holdout")
)
# use the test set as validation data during tuning
set_validate(at, validate = "test")
at$learner$validate
#> [1] "test"
```
