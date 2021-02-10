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

#' @title Syntactic Sugar for Tuning
#' 
#' @description
#' Sugar
#' 
#' @export 
tune = function(method, task, learner, resampling, measures, term_evals = NULL, term_time = NULL, 
  search_space = NULL, ...) {
  assert_choice(method, mlr_tuners$keys())

  terminator = if (!is.null(term_evals)) {
    trm("evals", n_evals = term_evals)
  } else if (!is.null(term_time)) {
    trm("run_time", secs = term_time)
  } else {
    stop("Either `term_evals` or `term_time` must be provided")
  }

  tuner = tnr(method, ...)
  instance = TuningInstanceSingleCrit$new(task, learner, resampling, measures, terminator, 
    search_space)
  tuner$optimize(instance)
  instance
}

#' @title Syntactic Sugar for Automatic Tuning
#' 
#' @description
#' Sugar
#' 
#' @export 
tune_auto = function(method, learner, resampling, measure, search_space = NULL, term_evals = NULL, 
  term_time = NULL, ...) {
    assert_choice(method, mlr_tuners$keys())

    terminator = if (!is.null(term_evals)) {
      trm("evals", n_evals = term_evals)
    } else if (!is.null(term_time)) {
      trm("run_time", secs = term_time)
    } else {
      stop("Either `term_evals` or `term_time` must be provided")
    }

    tuner = tnr(method, ...)
    AutoTuner$new(learner, resampling, measure, terminator, tuner, search_space)
  }

#' @title Syntactic Sugar for Nested Resampling
#' 
#' @description
#' Sugar
#' 
#' @export 
tune_nested = function(method, task, learner, inner_resampling, outer_resampling, measure, 
  term_evals = NULL, term_time = NULL, search_space = NULL, ...) {
    assert_task(task)
    assert_resampling(inner_resampling)
    assert_resampling(outer_resampling)
    assert_choice(method, mlr_tuners$keys())

    terminator = if (!is.null(term_evals)) {
      trm("evals", n_evals = term_evals)
    } else if (!is.null(term_time)) {
      trm("run_time", secs = term_time)
    } else {
      stop("Either `term_evals` or `term_time` must be provided")
    }

    tuner = tnr(method, ...)
    at = tune_auto(method, learner, inner_resampling, measure, search_space, term_evals, term_time, ...)
    resample(task, at, outer_resampling, store_models = TRUE)
  }
