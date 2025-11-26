# One Standard Error Rule Callback

The one standard error rule takes the number of features into account
when selecting the best hyperparameter configuration. Many learners
support internal feature selection, which can be accessed via
`$selected_features()`. The callback selects the hyperparameter
configuration with the smallest feature set within one standard error of
the best performing configuration. If there are multiple such
hyperparameter configurations with the same number of features, the
first one is selected.

## Source

Kuhn, Max, Johnson, Kjell (2013). “Applied Predictive Modeling.” In
chapter Over-Fitting and Model Tuning, 61–92. Springer New York, New
York, NY. ISBN 978-1-4614-6849-3.

## Examples

``` r
clbk("mlr3tuning.one_se_rule")
#> <CallbackBatchTuning:mlr3tuning.one_se_rule>: One Standard Error Rule Callback
#> * Active Stages: on_tuning_result_begin, on_eval_before_archive,
#>   on_optimization_begin

# Run optimization on the pima data set with the callback
instance = tune(
  tuner = tnr("random_search", batch_size = 15),
  task = tsk("pima"),
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)),
  resampling = rsmp("cv", folds = 3),
  measures = msr("classif.ce"),
  term_evals = 30,
  callbacks = clbk("mlr3tuning.one_se_rule")
)

# Hyperparameter configuration with the smallest feature set within one standard error of the best
instance$result
#>          cp n_features learner_param_vals  x_domain classif.ce
#>       <num>      <num>             <list>    <list>      <num>
#> 1: -4.78003   5.666667          <list[2]> <list[1]>  0.2252604
```
