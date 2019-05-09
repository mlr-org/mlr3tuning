#' @export
as.data.frame.FederatedCompBoost = function(fed_cboost) {
  iters_ids = unwrap(fed_cboost, "id")
  iters_coefs = unwrap(fed_cboost, "model", function(obj) {
    if (inherits(obj, "lm")) {
      return(coef(obj))
    } else {
      return(obj)
    }
  })

  iters_ids_df = lapply(iters_ids, function(iter) {
    cbind(tidyr::gather(as.data.frame(iter), "client", "feature"), iteration_client = seq_len(nrow(iter)))
  })
  iters_coefs_df = lapply(iters_coefs, function(iter) {
    cbind(tidyr::gather(as.data.frame(iter), "client", "coefs"), iteration_client = seq_len(nrow(iter)))
  })
  df_ids = do.call(rbind, lapply(names(iters_ids_df), function(iter_name) {
    cbind(iters_ids_df[[iter_name]], iteration = iter_name)
  }))
  df_coefs = do.call(rbind, lapply(names(iters_coefs_df), function(iter_name) {
    cbind(iters_coefs_df[[iter_name]], iteration = iter_name)
  }))
  dplyr::left_join(df_ids, df_coefs, by = c("client", "iteration", "iteration_client"))
}
