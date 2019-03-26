messageCondition = function(msg, ..., class = NULL, call = NULL) {
  structure(list(message = msg, call = call, ...), class = c(class, "message", "condition"))
}
