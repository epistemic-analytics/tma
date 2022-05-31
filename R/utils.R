# Adjacency Key
#
# @param codes character vector
# @param upper logical, default, TRUE, returns the key for the upper triangle, FALSE - not implemented yet
#
# @return matrix
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


as.ena.directed.set <- function(x) {
  to <- c("ena.directed.set")
  if(!inherits(x = x, what = "ena.set")) {
    to <- c(to, "ena.set")
  }
  class(x) <- c(to, class(x));
  return(x);
}
ena.set.directed <- function(data, units, conversations, codes, ...) {
  as.ena.directed.set(
    list(
      meta.data = NULL,
      model = list(
        model.type = "Directed",
        raw.input = data.table::as.data.table(data)
      ),
      rotation = list (
        codes = codes,
        nodes = NULL
      ),
      plots = list(),
      "_function.params" = list(
        units = units,
        conversations = conversations,
        codes = codes
      )
    )
  )
}

# Reclass vector
#
# @param x vector
# @param cl character - new class
# @param stringsAsFactors logical, default: default.stringsAsFactors()
#
# @return vector with class: c(cl, class(x))
reclass <- function(x, cl, stringsAsFactors = FALSE) {
  if(!stringsAsFactors && is.factor(x)) {
    x <- as.character(x)
  }
  class(x) = c(cl, class(x))
  x
}

# paste columns to together
#
# @param x coerced to data.frame prior to subseting cols
# @param cols character vector - default: colnames(x)
# @param sep character default "."
#
# @return character vector
merge_columns <- function(x, cols = colnames(x), sep = ".") {
  do.call(paste, c(as.data.frame(x)[, cols, drop = FALSE], sep = sep))
}