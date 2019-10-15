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


#' @description calculate pareto front of a matrix
#'   also works for >2D pareto front
#' @example data = rbind(c(1, 0.25, 0.75, 0), c(0, 0.25, 0.75, 1))
#'   calculate_pareto_front(data)
#'   data = rbind(c(1,1,0,1), c(1,1,0,0), c(1,0,1,0), c(1,1,1,1))
#'   calculate_pareto_front(data, maximize = FALSE)
calculate_pareto_front = function(points, maximize = TRUE) {

  assert_matrix(points, types = "numeric")
  assert_logical(maximize)

  # temporarily transpose; re-transpose at the end of the function
  points = t(points)

  # sort all the points and go stepwise through the rows whenever
  # entries are tied; break ties in the end with noise data
  sorted = do.call(
    order,
    c(as.data.frame(points), runif(nrow(points))),
    list(decreasing = maximize)
  )
  sorted_data = points[sorted, ]

  # function for cummulative min or max of vector
  cummaxmin = function(x, m) if (m) cummax(x) else cummin(x)
   
  # flag non-violations of strict monotony in each column
  bool_matrix = mapply(function(x, m) !duplicated(cummaxmin(x, m)), sorted_data, maximize)
  # check for any flag in each row
  front = apply(bool_matrix, 1, any)
  # get original indices
  revert_sort = order(sorted)

  result = revert_sort[front]
  # stabilize algorithm by not switching up the order of the front members
  result = t(sort(result))

  return(result)
}

 
