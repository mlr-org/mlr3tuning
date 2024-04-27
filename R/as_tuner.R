#' @title Convert to a Tuner
#'
#' @description
#' Convert object to a [Tuner] or a list of [Tuner].
#'
#' @param x (any)\cr
#'   Object to convert.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @export
as_tuner = function(x, ...) { # nolint
  UseMethod("as_tuner")
}

#' @export
#' @param clone (`logical(1)`)\cr
#'  Whether to clone the object.
#' @rdname as_tuner
as_tuner.Tuner = function(x, clone = FALSE, ...) { # nolint
  if (isTRUE(clone)) x$clone() else x
}

#' @export
#' @rdname as_tuner
as_tuners = function(x, ...) { # nolint
  UseMethod("as_tuners")
}

#' @export
#' @rdname as_tuner
as_tuners.default = function(x, ...) { # nolint
  list(as_tuner(x, ...))
}

#' @export
#' @rdname as_tuner
as_tuners.list = function(x, ...) { # nolint
  lapply(x, as_tuner, ...)
}
