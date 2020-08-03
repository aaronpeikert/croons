lav_vars <- function(model){
  unique(unlist(model@pta$vnames$ov))
}
lav_vars_lv <- function(model){
  unique(unlist(model@pta$vnames$lv))
}
