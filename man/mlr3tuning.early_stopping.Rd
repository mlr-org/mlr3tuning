% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlr_callbacks.R
\name{mlr3tuning.early_stopping}
\alias{mlr3tuning.early_stopping}
\title{Early Stopping Callback}
\description{
This \link{CallbackTuning} integrates early stopping into the hyperparameter tuning of an XGBoost learner.
Early stopping estimates the optimal number of trees (\code{nrounds}) for a given hyperparameter configuration.
Since early stopping is performed in each resampling iteration, there are several optimal \code{nrounds} values.
The callback writes the maximum value to the archive in the \code{max_nrounds} column.
In the best hyperparameter configuration (\code{instance$result_learner_param_vals}), the value of \code{nrounds} is replaced by \code{max_nrounds} and early stopping is deactivated.
}
\details{
Currently, the callback does not work with \code{GraphLearner}s from the package \CRANpkg{mlr3pipelines}.
The callback is compatible with the \link{AutoTuner}.
The final model is fitted with the best hyperparameter configuration and \code{max_nrounds} i.e. early stopping is not performed.
}
\section{Resources}{

\itemize{
\item \href{https://mlr-org.com/gallery/optimization/2022-11-04-early-stopping-with-xgboost/}{gallery post} on early stopping with XGBoost.
}
}

\examples{
clbk("mlr3tuning.early_stopping")
\donttest{
if (requireNamespace("mlr3learners") && requireNamespace("xgboost") ) {
  library(mlr3learners)

  # activate early stopping on the test set and set search space
  learner = lrn("classif.xgboost",
    eta = to_tune(1e-02, 1e-1, logscale = TRUE),
    early_stopping_rounds = 5,
    nrounds = 100,
    early_stopping_set = "test")

  # tune xgboost on the pima data set
  instance = tune(
    tuner = tnr("random_search"),
    task = tsk("pima"),
    learner = learner,
    resampling = rsmp("cv", folds = 3),
    measures = msr("classif.ce"),
    term_evals = 10,
    callbacks = clbk("mlr3tuning.early_stopping")
  )
}
}
}
