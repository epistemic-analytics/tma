#' Adjacency Key
#'
#' @param codes character vector
#' @param upper logical, default, TRUE, returns the key for the upper triangle, FALSE - not implemented yet
#'
#' @return matrix
adjacency_key <- function(codes, upper = TRUE) {
  n <- length(codes);
  if(upper == TRUE) {
    matrix(c(
      codes[sequence((1:n-1))],
      codes[rep((2:n), times = (1:(n-1)))]
    ), byrow = TRUE, nrow = 2)
  }
  else {
    stop("Not implemented")
  }
}

list_vec_apply <- Vectorize(FUN = function(x, y = NULL, fn) {
  fn(x, y)
}, vectorize.args = c("x", "y"), SIMPLIFY = FALSE);


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


#' Reclass vector
#'
#' @param x vector
#' @param cl character - new class
#' @param stringsAsFactors logical, default: default.stringsAsFactors()
#'
#' @return vector with class: c(cl, class(x))
reclass <- function(x, cl, stringsAsFactors = default.stringsAsFactors()) {
  if(!stringsAsFactors && is.factor(x)) {
    x <- as.character(x)
  }
  class(x) = c(cl, class(x))
  x
}