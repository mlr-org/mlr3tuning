messageCondition = function(msg, ..., class = NULL, call = NULL) {
  structure(list(message = msg, call = call, ...), class = c(class, "message", "condition"))
}

# get an object from a list of id-able-objects, like many from mlr3
# returns NULL if not found
get_by_id = function(xs, id) {
  for (x in xs) {
    if(x$id == id) {
      return(x)
    }
  }
  return(NULL)
}
