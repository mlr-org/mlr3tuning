#' @export
Callbacks = R6Class("Callbacks",
  private = list(
    # called in ObjectiveTuning
    .on_eval_many_start = function(xss, resampling, objective) {
      invisible(NULL)
    },

    .on_eval_many_end = function(ydt, bmr, design, learners, objective) {
      invisible(NULL)
    },

    # called in ObjectiveTuningAsync
    .on_eval_start = function(xs, resampling, objective) {
      invisible(NULL)
    },

    .on_eval_end = function(ys, rr, objective) {
      return(ys)
    }
  ),

  public = list(
    on_eval_many_start = function(xss, resampling, objective) {
      private$.on_eval_many_start(xss, resampling, objective)
    },

    on_eval_many_end = function(ydt, bmr, design, learners, objective) {
      private$.on_eval_many_end(ydt, bmr, design, learners, objective)
    },

    on_eval_start = function(xs, resampling, objective) {
      private$.on_eval_start(xs, resampling, objective)
    },

    on_eval_end = function(ys, rr, objective) {
      private$.on_eval_end(ys, rr, objective)
    }
  )
)
