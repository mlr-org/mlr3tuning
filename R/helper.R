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


# calculate pareto front of an all-numeric data frame (or matrix)
# also works for >2D pareto front
pareto_front = function(data_frame, maximize = TRUE) {

    assert_data_frame(data_frame, types = "numeric")
    assert_logical(maximize)

    cummaxmin = if (maximize) cummax else cummin
    
    # prepare each column as comma seperated argument
    arg = paste0("data_frame[[", 1:ncol(data_frame), "]]", collapse = ", ")
    # sort the whole data.frame and go stepwise through the columns whenever
    # entries are tied
    code = paste0("order(", arg, ", decreasing = ", maximize, ")")
    sorted = eval(parse(text = code))
    sorted_data = data_frame[sorted, ]
    last_col = sorted_data[[length(sorted_data)]]
    # remove all entries that violate sort-inverted monotony in their last col
    front = sorted_data[!duplicated(cummaxmin(last_col)), ]

    return(front)
}


 
