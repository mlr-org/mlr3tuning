% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ObjectiveTuningAsync.R
\name{ObjectiveTuningAsync}
\alias{ObjectiveTuningAsync}
\title{Class for Tuning Objective}
\description{
Stores the objective function that estimates the performance of hyperparameter configurations.
This class is usually constructed internally by the \link{TuningInstanceBatchSingleCrit} or \link{TuningInstanceBatchMultiCrit}.
}
\section{Super classes}{
\code{\link[bbotk:Objective]{bbotk::Objective}} -> \code{\link[mlr3tuning:ObjectiveTuning]{mlr3tuning::ObjectiveTuning}} -> \code{ObjectiveTuningAsync}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-ObjectiveTuningAsync-clone}{\code{ObjectiveTuningAsync$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="eval"><a href='../../bbotk/html/Objective.html#method-Objective-eval'><code>bbotk::Objective$eval()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="eval_dt"><a href='../../bbotk/html/Objective.html#method-Objective-eval_dt'><code>bbotk::Objective$eval_dt()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="eval_many"><a href='../../bbotk/html/Objective.html#method-Objective-eval_many'><code>bbotk::Objective$eval_many()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="format"><a href='../../bbotk/html/Objective.html#method-Objective-format'><code>bbotk::Objective$format()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="help"><a href='../../bbotk/html/Objective.html#method-Objective-help'><code>bbotk::Objective$help()</code></a></span></li>
<li><span class="pkg-link" data-pkg="bbotk" data-topic="Objective" data-id="print"><a href='../../bbotk/html/Objective.html#method-Objective-print'><code>bbotk::Objective$print()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mlr3tuning" data-topic="ObjectiveTuning" data-id="initialize"><a href='../../mlr3tuning/html/ObjectiveTuning.html#method-ObjectiveTuning-initialize'><code>mlr3tuning::ObjectiveTuning$initialize()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-ObjectiveTuningAsync-clone"></a>}}
\if{latex}{\out{\hypertarget{method-ObjectiveTuningAsync-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ObjectiveTuningAsync$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
