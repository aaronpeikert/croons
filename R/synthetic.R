#'  Generates Sythethic Data
#'
#' Transforms factor scores to fit specific covariance matrices and combines them.
#'
#' @param within within data that should be retrofit. Most likely output from [`fs_within`].
#' @param between between data that should be retrofit. Most likely output from [`fs_between`].
#' @param within_cov the expected cov of within
#' @param between_cov the expected cov of between
#' @param ... one or more cluster identifier
#' @export

synthethic_data <- function(within, between, within_cov, between_cov, id, ...){
  dots <- enquos(...)
  id <- enquo(id)
  within_mat <- dplyr::select(within, -c(!!!dots, !!id))
  between_mat <- dplyr::select(between, -c(!!!dots, !!id))
  within_mat_t <- purrr::map_dfr(split(within_mat, dplyr::pull(within, !!id)), scale2cov, within_cov)
  between_mat_t <- scale2cov(between_mat, between_cov)
  within_t <- dplyr::mutate(within, !!!within_mat_t)
  between_t <- dplyr::mutate(between, !!!between_mat_t)
  combined <- dplyr::left_join(within_t, between_t, by = as_name(id), suffix = c("_uniquewithin", c("_uniquebetween")))
  combined <- dplyr::select(combined, -c(!!!dots, !!id))
  combined <- dplyr::select(combined, dplyr::ends_with("_uniquebetween")) + dplyr::select(combined, dplyr::ends_with("_uniquewithin"))
  combined <- dplyr::rename_all(combined, ~gsub("_uniquebetween", "", .x))
  dplyr::mutate(within, !!!combined)
}
sqrtm <- function(m, symmetric = TRUE){
  e <- eigen(m, symmetric = symmetric)
  v <- e$vectors
  v %*% diag(sqrt(e$values)) %*% t(v)
}

scale2cov <- function(mat, cov, allow_failure = TRUE){
  mat_names <- names(mat)
  mat <- as.matrix(mat)
  cov <- as.matrix(cov)
  if(allow_failure){
    out <- tryCatch(t(solve(sqrtm(stats::var(mat))) %*% t(mat)) %*% sqrtm(cov),
                    error = function(e)mat)
  } else {
    out <- t(solve(sqrtm(stats::var(mat))) %*% t(mat)) %*% sqrtm(cov)
  }

  out <- as.data.frame(out)
  names(out) <- mat_names
  out
}
