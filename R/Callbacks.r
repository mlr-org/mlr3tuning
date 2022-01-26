#' @export
Callbacks = R6Class("Callbacks",
  private = list(
    .on_eval_many_start = function(xss, resampling, self) {
      invisible(NULL)
    },

    .on_eval_many_end = function(ydt, bmr, design, learners, self) {
      invisible(NULL)
    }
  ),

  public = list(
    on_eval_many_start = function(xss, resampling, self) {
      private$.on_eval_many_start(xss, resampling, self)
    },

    on_eval_many_end = function(ydt, bmr, design, learners, self) {
      private$.on_eval_many_end(ydt, bmr, design, learners, self)
    }
  )
)
