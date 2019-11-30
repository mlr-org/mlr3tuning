terminated_error = function(instance) {
  msg = sprintf(
    fmt = "TuningInstance (tsk:%s, lrn:%s, term:%s) terminated",
    instance$task$id,
    instance$learner$id,
    format(instance$terminator)
  )

  set_class(list(message = msg, call = NULL), c("terminated_error", "error", "condition"))
}

# get an object from a list of id-able-objects, like many from mlr3
# returns NULL if not found
get_by_id = function(xs, id) {
  for (x in xs) {
    if (x$id == id) {
      return(x)
    }
  }
  return(NULL)
}


#' @title Return pareto front
#' @description calculate pareto front of a matrix
#'   also works for >2D pareto front
#' @param points Numeric matrix with each column corresponding to a point
#' @param maximize Bool vector of length one or nrow of points
#' @examples
#' \dontrun{
#' data = rbind(c(1, 0.25, 0.75, 0), c(0, 0.25, 0.75, 1))
#' calculate_pareto_front(data)
#' data = rbind(c(1,1,0,1), c(1,1,0,0), c(1,0,1,0), c(1,1,1,1))
#' calculate_pareto_front(data, maximize = FALSE)
#' }
#' @importFrom stats runif
calculate_pareto_front = function(points, maximize = TRUE) {

  assert_matrix(points, mode = "numeric")
  assert(
    check_logical(maximize, len = 1),
    check_logical(maximize, len = nrow(points))
  )

  colnames(points) = seq_len(ncol(points))
  # temporarily transpose; re-transpose at the end of the function
  points = t(points)

  # sort all the points and go stepwise through the dims whenever
  # entries are tied; break ties in the end with noise data
  sorted = do.call(
    order,
    c(as.list(as.data.frame(points)), list(runif(nrow(points))),
    list(decreasing = maximize))
  )
  sorted_data = points[sorted, , drop = FALSE]

  # function for cummulative min or max of vector
  cummaxmin = function(x, m) if (m) cummax(x) else cummin(x)
   
  # flag non-violations of strict monotony in each column
  bool_matrix = as.matrix(mapply(
    function(x, m) !duplicated(cummaxmin(x, m)),
    as.data.frame(sorted_data),
    maximize
  ))
  # check for any flag in each row
  front = apply(bool_matrix, 1, any)
  # get original indices
  revert_sort = as.numeric(rownames(sorted_data))
  # select the front in the original indices
  result = revert_sort[front]
  # stabilize algorithm by not switching up the order of the front members
  return(sort(result))
}

 
