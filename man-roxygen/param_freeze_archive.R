#' @param freeze_archive (`logical(1)`)\cr
#' If `TRUE`, the archive is copied from redis to a local data.table after tuning.
#' This is helpful when the tuning is run on a remote machine and the archive is serialized.
#' Only used if `rush` is supplied.
