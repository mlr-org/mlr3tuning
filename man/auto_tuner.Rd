% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auto_tuner.R
\name{auto_tuner}
\alias{auto_tuner}
\title{Syntactic Sugar for Automatic Tuning}
\usage{
auto_tuner(
  method,
  learner,
  resampling,
  measure,
  term_evals = NULL,
  term_time = NULL,
  search_space = NULL,
  ...
)
}
\arguments{
\item{method}{(\code{character(1)})\cr
Key to retrieve tuner from \link{mlr_tuners} dictionary.}

\item{learner}{(\link[mlr3:Learner]{mlr3::Learner}).}

\item{resampling}{(\link[mlr3:Resampling]{mlr3::Resampling})\cr
Uninstantiated resamplings are instantiated during construction
so that all configurations are evaluated on the same data splits. If a new
resampling is passed, it is instantiated with new data splits. Already
instantiated resamplings are kept unchanged.}

\item{measure}{(\link[mlr3:Measure]{mlr3::Measure})\cr
Measure to optimize.}

\item{term_evals}{(\code{integer(1)})\cr
Number of allowed evaluations.}

\item{term_time}{(\code{integer(1)})\cr
Maximum allowed time in seconds.}

\item{search_space}{(\link[paradox:ParamSet]{paradox::ParamSet})\cr
Hyperparameter search space. If \code{NULL}, the search space is constructed from
the \link{TuneToken} in the \code{ParamSet} of the learner.}

\item{...}{(named \code{list()})\cr
Named arguments to be set as parameters of the tuner.}
}
\value{
\link{AutoTuner}
}
\description{
Function to create an \link{AutoTuner} object.
}
\examples{
at = auto_tuner(
  method = "random_search",
  learner = lrn("classif.rpart", cp = to_tune(1e-04, 1e-1, logscale = TRUE)), 
  resampling = rsmp ("holdout"),
  measure = msr("classif.ce"), 
  term_evals = 4)  

at$train(tsk("pima"))
}
