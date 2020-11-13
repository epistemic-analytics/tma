list_vec_apply <- Vectorize(FUN = function(x, y = NULL, fn) {
  fn(x, y)
}, vectorize.args = c("x", "y"), SIMPLIFY = FALSE);
list_vec_apply_s <- Vectorize(FUN = function(x, y = NULL, fn) {
  fn(x, y)
}, vectorize.args = c("x", "y"), SIMPLIFY = TRUE);
get.decay.modes.vec <- Vectorize(
  FUN = get.decay.modes,
  vectorize.args = c("now", "event.time", "modes"),
  SIMPLIFY = FALSE
);
apply_decay <- function(x, d) {
    # Some additional params used for diag() to account for decays of length 1
    t(t(x) %*% diag(d, nrow = length(d), ncol = length(d)))
}
as.undirected.matrix <- function(x, m = matrix(1, nrow = nrow(x), ncol = ncol(x))) {
  upper_tri_indices <- upper.tri(x);
  ut_matrix <- x * m;
  ut_matrix[upper_tri_indices] <- ut_matrix[upper_tri_indices] + t(ut_matrix)[upper_tri_indices]
  
  #make matrix symmetrical
  ut_matrix[lower.tri(ut_matrix)] = NA
  
  ut_matrix
}
as.undirected.vector <- function(x, diag = FALSE) {
  upper_tri_indices <- upper.tri(x, diag = diag);
  x[upper_tri_indices]
}
