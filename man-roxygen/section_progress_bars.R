#' @section Progress Bars:
#' `$optimize()` supports progress bars via the package \CRANpkg{progressr}
#' combined with a [Terminator]. Simply wrap the function in
#' `progressr::with_progress()` to enable them. We recommend to use package
#' \CRANpkg{progress} as backend; enable with `progressr::handlers("progress")`.
