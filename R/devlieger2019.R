# the functions in this file were contributed by Caroline Gahrmann
# functions in correct_cov extract the model parameters
# and scale them across all entries

croon_within_var_ <- function(a_w, lambda_w, theta_w, var_f_w) {
  # direct translation of formula 21
  solve(a_w %*% lambda_w) * (var_f_w - a_w %*% theta_w %*% t(a_w)) * solve(t(lambda_w) %*% t(a_w))
}

croon_within_cov_ <- function(a_w, lambda_w, cov_f_w){
  # direct translation of formula 22
  solve(a_w[[1]] %*% lambda_w[[1]]) * cov_f_w * solve(t(lambda_w[[2]]) %*% t(a_w[[2]]))
}

