multi_predict <- function(models, data, level = 1, ..., fsm = FALSE){
  dots <- enquos(...)
  if(length(dots) == 0L)stop("Please supply at least one id variable.", call. = FALSE)
  vars <- purrr::as_vector(purrr::map(models, lav_vars), "character")
  data <- dplyr::select(data, !!!dots, dplyr::all_of(vars))
  data <- na.omit(data)
  scores <- purrr::map(models, lavaan::lavPredict, type = "lv", newdata = data, level = level, method = "regression", fsm = fsm)
  if(!fsm){
    scores <- purrr::map_dfc(scores, as.data.frame)
    if(as.character(level) == "1") {
      out <- dplyr::mutate(dplyr::select(data, !!!dots), !!!scores)
    } else if(as.character(level) == "2") {
      out <- dplyr::mutate(dplyr::distinct(dplyr::select(data, !!!dots)), !!!scores)
    } else {
      stop("Level must be either 1 or 2")
    }
  } else {
    fsm <- purrr::flatten(purrr::map(scores, attr, "fsm"))
    names(fsm) <- purrr::map_chr(fsm, row.names)
    out <- fsm
  }
  out
}
#' Calculate factor scores
#'
#' Wrapper for [`lavaan::lavPredict`].for multiple models.
#' @param models fitted multilevel lavaan models e.g. output of [`fit_models`]
#' @param data the dataset on which the models where fitted, rows which contain missings on the variables within models or ... are dropped.
#' @param ... additional variables that should be retained
#' @rdname fs
#' @seealso [fsm]

#' @name fs
#' @export
fs_within <- function(models, data, ...){
  multi_predict(models, data, level = 1, fsm = FALSE, ...)
}

#' @name fs
#' @export
fs_between <- function(models, data, ...){
  multi_predict(models, data, level = 2, fsm = FALSE, ...)
}

#' Calculate Factor Score Matrices
#'
#' Wrapper for [`lavaan::lavPredict`] (with `fsm = TRUE`) for multiple models.
#' @inheritParams fs
#' @rdname fsm
#' @seealso [fs]

#' @name fsm
#' @export
fsm_within <- function(models, data, ...){
  multi_predict(models, data, level = 1, fsm = TRUE, ...)
}

#' @name fsm
#' @export
fsm_between <- function(models, data, ...){
  multi_predict(models, data, level = 2, fsm = TRUE, ...)
}
