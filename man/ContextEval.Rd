% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ContextEval.R
\name{ContextEval}
\alias{ContextEval}
\title{Evaluation Context}
\description{
The \link{ContextEval} allows \link{CallbackTuning}s to access and modify data while a batch of hyperparameter configurations is evaluated.
See section on active bindings for a list of modifiable objects.
See \code{\link[=callback_tuning]{callback_tuning()}} for a list of stages which access \link{ContextEval}.
}
\details{
This context is re-created each time a new batch of hyperparameter configurations is evaluated.
Changes to \verb{$objective_tuning}, \verb{$design} \verb{$benchmark_result} are discarded after the function is finished.
Modification on the data table in \verb{$aggregated_performance} are written to the archive.
Any number of columns can be added.
}
\section{Super class}{
\code{\link[mlr3misc:Context]{mlr3misc::Context}} -> \code{ContextEval}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{objective_tuning}}{\link{ObjectiveTuning}.}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{xss}}{(list())\cr
The hyperparameter configurations of the latest batch.
Contains the values on the learner scale i.e. transformations are applied.
See \verb{$xdt} in \link[bbotk:ContextOptimization]{bbotk::ContextOptimization} for the untransformed values.}

\item{\code{design}}{(\link[data.table:data.table]{data.table::data.table})\cr
The benchmark design of the latest batch.}

\item{\code{benchmark_result}}{(\link[mlr3:BenchmarkResult]{mlr3::BenchmarkResult})\cr
The benchmark result of the latest batch.}

\item{\code{aggregated_performance}}{(\link[data.table:data.table]{data.table::data.table})\cr
Aggregated performance scores and training time of the latest batch.
This data table is passed to the archive.
A callback can add additional columns which are also written to the archive.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ContextEval-new}{\code{ContextEval$new()}}
\item \href{#method-ContextEval-clone}{\code{ContextEval$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mlr3misc" data-topic="Context" data-id="format"><a href='../../mlr3misc/html/Context.html#method-Context-format'><code>mlr3misc::Context$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3misc" data-topic="Context" data-id="print"><a href='../../mlr3misc/html/Context.html#method-Context-print'><code>mlr3misc::Context$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ContextEval-new"></a>}}
\if{latex}{\out{\hypertarget{method-ContextEval-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ContextEval$new(objective_tuning)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{objective_tuning}}{\link{ObjectiveTuning}.}

\item{\code{id}}{(\code{character(1)})\cr
Identifier for the new callback.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ContextEval-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ContextEval-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ContextEval$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
