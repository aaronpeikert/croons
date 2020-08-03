croon_within_cov <- function(models, a_w, s_f_w){
  # wrapper of 22 to extract correct variables
  stopifnot(length(models) == 2L)
  lv <- purrr::map_chr(models, lav_vars_lv)
  a_w <- a_w[lv]
  lambda_w <- purrr::map(models, ~lavInspect(.x, "est")$within$lambda)
  which_cov <- purrr::map(lv, ~.x == colnames(s_f_w))
  cov_f_w <- s_f_w[which_cov[[1]], ][which_cov[[2]]]
  two_numeric <- function(x)(length(x) == 2L) &&
    is.numeric(x[[1]]) &&
    is.numeric(x[[2]])
  stopifnot(two_numeric(a_w))
  stopifnot(two_numeric(lambda_w))
  stopifnot(is.numeric(cov_f_w), length(cov_f_w) == 1L)
  croon_within_cov_(a_w, lambda_w, cov_f_w)
}

#' @export
croon_within_covs <- function(models, s_f_w, a_w){
  # scale 22 to all covs
  models <- utils::combn(models, 2, simplify = FALSE)
  lv <- purrr::map_depth(models, 2, lav_vars_lv)
  covs <- purrr::map(models, croon_within_cov, a_w, s_f_w)
  for(i in seq_along(covs)){
    which_cov <- purrr::map(lv[[i]], ~.x == colnames(s_f_w))
    s_f_w[which_cov[[1]], ][which_cov[[2]]] <- covs[[i]]
    s_f_w[which_cov[[2]], ][which_cov[[1]]] <- covs[[i]]
  }
  s_f_w
}
