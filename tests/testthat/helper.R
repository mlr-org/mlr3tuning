library(mlr3)
library(checkmate)
library(mlr3misc)
library(mlr3pipelines)
library(paradox)
library(R6)
library(rush)

lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)
lapply(list.files(system.file("testthat", package = "mlr3tuning"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

sortnames = function(x) {
  if (!is.null(names(x))) {
    x <- x[order(names(x), decreasing = TRUE)]
  }
  x
}
