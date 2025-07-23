library(mlr3)
library(checkmate)
library(mlr3misc)
library(paradox)
library(R6)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3tuning"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

sortnames = function(x) {
  if (!is.null(names(x))) {
    x <- x[order(names(x), decreasing = TRUE)]
  }
  x
}

# suppress warning "Canceling all iterations" from future.apply
expect_resample_error = function(object,
  regexp = NULL,
  class = NULL,
  ...,
  inherit = TRUE,
  info = NULL,
  label = NULL
  ) {
  withCallingHandlers(
    expect_error(object, regexp, class, ..., inherit = inherit, info = info, label = label),
    warning = function(w) {
      if (grepl("Canceling all iterations", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
