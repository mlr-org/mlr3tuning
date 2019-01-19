extract_min_evals = function (terminator, n_batch) {
  n_evals = terminator$settings$max_evaluations
  if (is.null(n_evals)) return(n_batch) else return(min(n_batch, n_evals))
}
