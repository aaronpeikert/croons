cov_unscaled <- function(mat, means){
  # unscaled covariance for formula 19/20
  mat <- as.matrix(mat)
  if(missing(means))means <- colMeans(mat)
  if(is.null(dim(means))){
    means <- matrix(rep(colMeans(mat), each = nrow(mat)), nrow = nrow(mat))
  }
  mat <- mat - means
  Reduce(`+`, lapply(split(mat, seq_len(nrow(mat))), function(x)x %*% t(x)))
}

#' Calculate the uncorrected within and between covariance.
#'
#' @param within the data on within level including an id
#' @param between the data on the between level optionally including an id
#' @param id the name of the variable that serves as an identifier
#' @rdname cov

#' @name cov
#' @export
within_cov <- function(within, between, id){
  # formula 20
  id <- rlang::enquo(id)
  ids <- dplyr::pull(within, !!id)
  clusters <- lapply(split(dplyr::select(within, -c(!!id)), ids), as.matrix)
  means <- lapply(split(dplyr::select(between, -c(!!id)), dplyr::pull(between, !!id)), unlist)
  out <- matrix(0, nrow = ncol(within) - 1, ncol = ncol(within) - 1)
  for(i in ids){
    out <- out + cov_unscaled(clusters[[i]], means[[i]])
  }
  out <- out / (nrow(within) - 1)
  colnames(out) <- names(means[[1]])
  out
}

#' @name cov
#' @export
between_cov <- function(between, id){
  # formula 19
  id <- enquo(id)
  between <- dplyr::select(between, -c(!!id))
  out <- cov_unscaled(between)/nrow(between)
  colnames(out) <- names(between)
  out
}
