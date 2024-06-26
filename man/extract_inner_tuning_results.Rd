% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract_inner_tuning_results.R
\name{extract_inner_tuning_results}
\alias{extract_inner_tuning_results}
\alias{extract_inner_tuning_results.ResampleResult}
\alias{extract_inner_tuning_results.BenchmarkResult}
\title{Extract Inner Tuning Results}
\usage{
extract_inner_tuning_results(x, tuning_instance, ...)

\method{extract_inner_tuning_results}{ResampleResult}(x, tuning_instance = FALSE, ...)

\method{extract_inner_tuning_results}{BenchmarkResult}(x, tuning_instance = FALSE, ...)
}
\arguments{
\item{x}{(\link[mlr3:ResampleResult]{mlr3::ResampleResult} | \link[mlr3:BenchmarkResult]{mlr3::BenchmarkResult}).}

\item{tuning_instance}{(\code{logical(1)})\cr
If \code{TRUE}, tuning instances are added to the table.}

\item{...}{(any)\cr
Additional arguments.}
}
\value{
\code{\link[data.table:data.table]{data.table::data.table()}}.
}
\description{
Extract inner tuning results of nested resampling.
Implemented for \link[mlr3:ResampleResult]{mlr3::ResampleResult} and \link[mlr3:BenchmarkResult]{mlr3::BenchmarkResult}.
}
\details{
The function iterates over the \link{AutoTuner} objects and binds the tuning results to a \code{\link[data.table:data.table]{data.table::data.table()}}.
The \link{AutoTuner} must be initialized with \code{store_tuning_instance = TRUE} and \code{\link[mlr3:resample]{mlr3::resample()}} or \code{\link[mlr3:benchmark]{mlr3::benchmark()}} must be called with \code{store_models = TRUE}.
Optionally, the tuning instance can be added for each iteration.
}
\section{Data structure}{


The returned data table has the following columns:
\itemize{
\item \code{experiment} (integer(1))\cr
Index, giving the according row number in the original benchmark grid.
\item \code{iteration} (integer(1))\cr
Iteration of the outer resampling.
\item One column for each hyperparameter of the search spaces.
\item One column for each performance measure.
\item \code{learner_param_vals} (\code{list()})\cr
Hyperparameter values used by the learner.
Includes fixed and proposed hyperparameter values.
\item \code{x_domain} (\code{list()})\cr
List of transformed hyperparameter values.
\item \code{tuning_instance} (\link{TuningInstanceBatchSingleCrit} | \link{TuningInstanceBatchMultiCrit})\cr
Optionally, tuning instances.
\item \code{task_id} (\code{character(1)}).
\item \code{learner_id} (\code{character(1)}).
\item \code{resampling_id} (\code{character(1)}).
}
}

\examples{
# Nested Resampling on Palmer Penguins Data Set

learner = lrn("classif.rpart",
  cp = to_tune(1e-04, 1e-1, logscale = TRUE))

# create auto tuner
at = auto_tuner(
  tuner = tnr("random_search"),
  learner = learner,
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"),
  term_evals = 4)

resampling_outer = rsmp("cv", folds = 2)
rr = resample(tsk("iris"), at, resampling_outer, store_models = TRUE)

# extract inner results
extract_inner_tuning_results(rr)
}
