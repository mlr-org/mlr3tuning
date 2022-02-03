#' @title Convert to a Search Space
#'
#' @description
#' Convert object to a search space.
#'
#' @param x (`any`)\cr
#'   Object to convert to search space.
#' @param ... (any)\cr
#'   Additional arguments.
#'
#' @return [paradox::ParamSet].
#' @export
as_search_space = function(x, ...) { # nolint
  UseMethod("as_search_space")
}

#' @export
#' @rdname as_search_space
as_search_space.Learner = function(x, ...) { # nolint
  x$param_set$search_space()
}

#' @export
#' @rdname as_search_space
as_search_space.ParamSet = function(x, ...) { # nolint
  if (length(x$get_values(type = "only_token"))) x$search_space() else x
}
