#' Simulate Multilevel Data
#'
#' Simulate 2-level data with [`lavaan::simulateData`].
#'
#' @param model a two-level sem (first level one, then level two). Anything that [`lavaan::lavaanify`] accepts.
#' @param nobs.l1 Number of observations on level 1. May be a vector of length nobs.l2
#' @param nobs.l2 Number of observations on level 2.
#' @param model.l1 a simple model only for level 1.
#' @param model.l2 a simple model only for level 2
#' @param data.l2 the level 2 data, most likely result of `simulate_data_l2`.
#' @param sample.nobs Number of observations.
#' @param id the cluster identifier
#' @param ... passed to [`lavaan::simulateData`]
#' @rdname simulate_data
#' @export
simulate_data <- function(model, nobs.l1 , nobs.l2, ...){
  # RVerbalExpressions
  # rx_whitespace() %>% rx_none_or_more() %>%
  #  rx_find("level:") %>% rx_whitespace() %>% rx_none_or_more() %>%
  #  rx_digit() %>% rx_whitespace() %>% rx_none_or_more()
  models <- strsplit(model, "\\s*(level\\:)\\s*\\d\\s*")[[1]]
  models <- models[models != ""]
  stopifnot(length(models) == 2L)
  simulate_data_l1(models[[1]], nobs.l1, simulate_data_l2(models[[2]], nobs.l2))
}

#' @name simulate_data
#' @export
simulate_data_l2 <- function(model.l2, sample.nobs, id, ...){
  stopifnot(length(sample.nobs) == 1L)
  data.l2 <-
    lavaan::simulateData(model = model.l2,
                         sample.nobs = sample.nobs,
                         return.type = "data.frame",
                         ...)
  if(missing(id))id <- seq_len(sample.nobs)
  dplyr::select(dplyr::mutate(data.l2, id = id), id, dplyr::everything())
}

#' @name simulate_data
#' @export
simulate_data_l1 <- function(model.l1, sample.nobs, data.l2, ...){
  if(length(sample.nobs) == 1L)rep(sample.nobs, NROW(data.l2))
  model.l1 <- rep(model.l1, NROW(data.l2))
  id <- dplyr::pull(data.l2, "id")
  data.l2 <- dplyr::select(data.l2, -dplyr::all_of("id"))
  data.l2 <- split(data.l2, seq_len(NROW(data.l2)))
  purrr::pmap_dfr(list(
    model.l1 = model.l1,
    sample.nobs = sample.nobs,
    data.l2 = data.l2,
    id = id
  ),
  simulate_data_l1_
  )
}

simulate_data_l1_ <- function(model.l1, sample.nobs, data.l2, id, ...){
  stopifnot(length(sample.nobs) == 1L)
  stopifnot(dim(data.l2)[[1]] == 1L)
  data.l1 <-
    lavaan::simulateData(model = model.l1,
                         sample.nobs = sample.nobs,
                         return.type = "data.frame",
                         ...)
  data.l2 <- dplyr::bind_rows(rep(list(data.l2), sample.nobs))
  data.l1 <- data.l1[names(data.l2)]
  combined <- data.l2 + data.l1
  dplyr::select(dplyr::mutate(combined, id = id), id, dplyr::everything())
}
