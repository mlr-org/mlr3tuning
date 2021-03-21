#' @title Syntactic Sugar for Tuner Construction
#'
#' @description
#' This function complements [mlr_tuners] with functions in the spirit
#' of [mlr3::mlr_sugar].
#'
#' @inheritParams mlr3::mlr_sugar
#' @return
#' * [Tuner] for `tnr()`
#' * list of [Tuner] for `tnrs()`
#' @export
#' @examples
#' tnr("random_search")
tnr = function(.key, ...) {
  dictionary_sugar(mlr_tuners, .key, ...)
}

#' @rdname tnr
#' @export
tnrs = function(.keys, ...) {
  dictionary_sugar_mget(mlr_tuners, .keys, ...)
}

#' @title Syntactic Sugar for Automatic Tuning
#' 
#' @description
#' Function to create an [AutoTuner] object.
#' 
#' @param method (`character(1)`)\cr
#'  Key to retrieve tuner from [mlr_tuners] dictionary.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the tuner.
#' 
#' @return [AutoTuner]
#' 
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_search_space
#' 
#' @export
#' @examples
#' learner = lrn("classif.rpart")
#' learner$param_set$values$minsplit = to_tune(1, 10)
#'
#' at = auto_tuner(
#'   method = "random_search",
#'   learner = learner, 
#'   resampling = rsmp ("holdout"),
#'   measure = msr("classif.ce"), 
#'   term_evals = 50, 
#'   batch_size = 10)  
#'
#' at$train(tsk("pima"))
auto_tuner = function(method, learner, resampling, measure, term_evals = NULL, term_time = NULL, search_space = NULL,
  ...) {
  assert_choice(method, mlr_tuners$keys())
  tuner = tnr(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  AutoTuner$new(learner, resampling, measure, terminator, tuner, search_space)
}

#' @title Function for Tuning
#' 
#' @description
#' Function to tune a [mlr3::Learner].
#' 
#' @param method (`character(1)`)\cr
#'  Key to retrieve tuner from [mlr_tuners] dictionary.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the tuner.
#' 
#' @return `TuningInstanceSingleCrit`
#'  
#' @template param_task
#' @template param_learner
#' @template param_resampling
#' @template param_measure
#' @template param_search_space
#' 
#' @export 
#' @examples
#' learner = lrn("classif.rpart")
#' learner$param_set$values$minsplit = to_tune(1, 10)
#'
#' instance = tune(
#'   method = "random_search", 
#'   task = tsk("pima"), 
#'   learner = learner, 
#'   resampling = rsmp ("holdout"), 
#'   measure = msr("classif.ce"), 
#'   term_evals = 50, 
#'   batch_size = 10) 
#' 
#' # Apply hyperparameter values to learner
#' learner$param_set$values = instance$result_learner_param_vals
tune = function(method, task, learner, resampling, measure, term_evals = NULL, term_time = NULL, search_space = NULL,
  ...) {
  assert_choice(method, mlr_tuners$keys())
  tuner = tnr(method, ...)
  terminator = terminator_selection(term_evals, term_time)

  instance = TuningInstanceSingleCrit$new(task, learner, resampling, measure, terminator, search_space)

  tuner$optimize(instance)
  instance
}

#' @title Function for Nested Resampling
#' 
#' @description
#' Function to conduct nested resampling.
#' 
#' @param method (`character(1)`)\cr
#'  Key to retrieve tuner from [mlr_tuners] dictionary.
#' @param inner_resampling ([mlr3::Resampling])\cr
#'  Resampling used for the inner loop.
#' @param outer_resampling [mlr3::Resampling])\cr
#'  Resampling used for the outer loop.
#' @param term_evals (`integer(1)`)\cr
#'  Number of allowed evaluations.
#' @param term_time (`integer(1)`)\cr
#'  Maximum allowed time in seconds.
#' @param ... (named `list()`)\cr
#'  Named arguments to be set as parameters of the tuner.
#' 
#' @return [mlr3::ResampleResult]
#'  
#' @template param_task
#' @template param_learner
#' @template param_measure
#' @template param_search_space
#' 
#' @export 
#' @examples
#' learner = lrn("classif.rpart")
#' learner$param_set$values$minsplit = to_tune(1, 10)
#'
#' rr = tune_nested(
#'   method = "random_search",
#'   task = tsk("pima"),
#'   learner = learner, 
#'   inner_resampling = rsmp ("holdout"),
#'   outer_resampling = rsmp("cv", folds = 2), 
#'   measure = msr("classif.ce"),
#'   term_evals = 2,
#'   batch_size = 2)
#' 
#' # check the inner results
#' extract_inner_tuning_results(rr)
#' 
#' # aggregate performance of outer results
#' rr$aggregate()
tune_nested = function(method, task, learner, inner_resampling, outer_resampling, measure, term_evals = NULL, 
  term_time = NULL, search_space = NULL, ...) {
  assert_task(task)
  assert_resampling(inner_resampling)
  assert_resampling(outer_resampling)

  at = auto_tuner(method, learner, inner_resampling, measure, search_space, term_evals, term_time, ...)
  resample(task, at, outer_resampling, store_models = TRUE)
}

terminator_selection = function(term_evals, term_time) {
  assert_int(term_evals, null.ok = TRUE)
  assert_int(term_time, null.ok = TRUE)

  if (is.null(term_evals) && is.null(term_time)) {
    trm("none")
  } else if (!is.null(term_evals) && !is.null(term_time)) {
    trm("combo", list(trm("evals", n_evals = term_evals), trm("run_time", secs = term_time)))
  } else if (!is.null(term_evals)) {
    trm("evals", n_evals = term_evals)
  } else if (!is.null(term_time)) {
    trm("run_time", secs = term_time)
  }
}
