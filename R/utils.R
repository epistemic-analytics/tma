# Adjacency Key
#
# @param codes character vector
# @param upper logical, default, TRUE, returns the key for the upper triangle, FALSE - not implemented yet
#
# @return matrix
adjacency_key <- function(codes, upper = TRUE) {
  expanded <- expand.grid(codes, codes);
  rownames(expanded) <- apply(expanded, 1, paste, collapse = " & ");
  if(upper == TRUE) {
    upper_inds <- which(upper.tri(matrix(nrow = length(codes), ncol = length(codes))));
    t(expanded[upper_inds,])
  }
  else {
    t(expanded);
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

#' Extract Upper Triangular Elements
#' 
#' This function extracts the elements from the upper triangular part of a
#' matrix.
#' 
#' @param x A numeric matrix from which to extract upper triangular elements.
#' @param diag A logical value indicating whether to include the diagonal
#'   elements. Defaults to FALSE.
#' 
#' @return A vector containing the upper triangular elements of the matrix.
as.undirected.vector <- function(x, diag = FALSE) {
  upper_tri_indices <- upper.tri(x, diag = diag);
  x[upper_tri_indices]
}


as.ena.ordered.set <- function(x) {
  to <- c("ena.ordered.set")
  if(!inherits(x = x, what = "ena.set")) {
    to <- c(to, "ena.set")
  }
  class(x) <- c(to, class(x));
  return(x);
}
ena.set.directed <- function(data, units, conversations, codes, ...) {
  as.ena.ordered.set(
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

#' TBD
#'
#' @param x TBD
#' @param ... TBD
#'
#' @return TBD
#' @export
'as.character.adjacency.key' <- function(x, ...) {
  # collapse = " & "
  # apply(x, 1, paste, collapse = " & ")
  mapply(paste, x[[1]], x[[2]], MoreArgs = list(sep = " & "))
}
#' TBD
#'
#' @param x TBD
#' @param ... TBD
#'
#' @return TBD
#' @export
'as.double.adjacency.key' <- function(x, ...) {
  sapply(x, as.numeric)
}

# @export
#' Title
#'
#' @param x TBD
#' @param include.meta TBD
#' @param ... TBD
#'
#' @return TBD
#' @export
'print.network.matrix' <- function(x, include.meta = TRUE, ...) {
  # print("Test: add the meta")
  # browser()
  x_ <- data.table::copy(x);
  x_model <- attr(x, "model");
  x_cls <- class(x);
  class(x_) <- x_cls[ (which(x_cls == "network.matrix") + 1):length(x_cls) ];
  if(!is.null(x_model)) {
    adj_key <- x_model$rotation$adjacency.key;
    attr(x_, "names") <- as.character(adj_key);
    # data.table:::print.data.table(data.table:::cbind.data.table(y$meta.data, y$networks)); 
  }
  
  print(x_);
}

#  Title
# 
#  @param x TBD
#  @param ... TBD
#  @param with.meta TBD
# 
#  @return TBD
# @export
# '[.network.matrix' <- function(
#   x, with.meta = FALSE, ...
# ) {
#   browser()
#   x_ <- data.table::copy(x);
#   x_cls <- class(x);
#   class(x_) <- x_cls[ (which(x_cls == "network.matrix") + 1):length(x_cls) ];
#   
#   # x_subset <- data.table:::`[.data.table`(x_, ...);
#   
#   # meta_ <- NULL;
#   meta_ <- attr(x, "model")$meta.data;
#   if(with.meta == TRUE) {
#     # meta_subset <- data.table:::`[.data.table`(attr(x, "model")$meta.data, ...);
#   
#     y_ <- data.table::cbind.data.table(meta_, x_);
#     
#     # browser()
#     y_subset <- data.table::`[.data.table`(y_, ...);
#   }
#   else {
#     y_rows <- data.table::`[.data.table`(meta_, ..., which = TRUE);
#     y_subset <- data.table::`[.data.table`(x_, y_rows);
#   }
# 
#   y_subset; 
# }


#' Extract from network.matrix easily using metadata
#'
#' @param x [TBD]
#' @param i [TBD]
#'
#' @return [TBD]
#' @export
"$.network.matrix" <- function (x, i) {
  meta.data <- attr(x, "model")$meta.data;
  meta.cols <- colnames(meta.data);
  
  # browser()
  if(data.table::`%chin%`(i, meta.cols)) {
    meta.data[[i]];
  }
  else {
    x[[i]];
  }
}

#' Title
#'
#' @param x TBD
#'
#' @return TBD
#' @export
"names.network.connections" <- function(x) {
  use.adjacency.key = getOption("tma.print.adjkey", TRUE);
  
  x_cls <- class(x);
  x_ <- data.table::copy(x);
  class(x_) <- x_cls[ (which(x_cls == "network.connections") + 1):length(x_cls) ]; 
  
  if(use.adjacency.key == TRUE) {
    x_model <- attr(x, "model");
    adj_key <- x_model$rotation$adjacency.key;
    as.character(adj_key);
  }
  else {
    names(x_);
  }
}


# Extract metadata easily
#
# @param x [TBD]
# @param i [TBD]
#
# @return [TBD]
# @export
# "$.network.metadata" <- function(x, i, which.rows = TRUE) {
#   browser()
#   parts <- unlist(strsplit(
#     x = as.character(sys.call())[2], split = "\\$"
#   ))[1:2];
#   
#   col <- as.character(sys.call(1)[[2]])[3];
#   val <- as.character(sys.call(1)[[3]]);
# 
#   set <- get(parts[1], envir = parent.frame())
#   m <- set[[parts[2]]];
#   rows <- as.vector(m[[col]]) == val;
#   if(which.rows == TRUE) {
#     rows;
#   }
#   else {
#     data.table:::`[.data.table`(m, rows);
#   }
# }



#' Re-class vector as network.connection
#'
#' @param x Vector to re-class
#'
#' @return re-classed vector
#' @export
as.network.connection <- function(x) {
  if(is.factor(x)) {
    x = as.character(x)
  }
  class(x) = c("network.connection", class(x))
  x
}

#' Title
#'
#' @param x TBD 
#' @param ... TBD
#'
#' @return TBD
#' @export
as.matrix.network.connections <- function(x, ...) {
  x_ <- data.table::copy(x);
  x_cls <- class(x);
  class(x_) <- x_cls[ (which(x_cls == "network.matrix") + 1):length(x_cls) ];
  code_columns <- which(sapply(x_, inherits, what = "network.connection"));
  as.matrix(x_[, c(code_columns), with = F], ...);
}

#' Title
#'
#' @param x 
#' @param na.rm 
#' @param dims 
#' @param binary 
#'
#' @returns
#' @export
'colSums.ena.matrix' <- function(x, na.rm = FALSE, dims = 1L, binary = FALSE) {
  x_mat <- as.matrix(x);
  if(isTRUE(binary)) {
    x_mat[x_mat > 0] <- 1;
  }
  colSums(x_mat);  
}

#' Unorder Connections in a Matrix
#'
#' This function takes a matrix and creates an unordered version of its connections, combining upper and lower triangular elements.
#'
#' @param x A matrix or data frame containing the connections. The input should be a square matrix.
#'
#' @return A data.table with ordered connections, reclassified as "unordered.ena.connections", "ena.connections", and "ena.matrix".
#'
#' @export
'as.unordered.ordered.ena.connections' <- function(x) {
  m <- as.matrix(x);
  sq_size <- sqrt(ncol(m));
  ind_mat <- matrix(seq.int(ncol(m)), nrow = sq_size, ncol = sq_size);
  ind_ut <- ind_mat[upper.tri(ind_mat)];
  ind_lt <- ind_mat[lower.tri(ind_mat)];
  
  # m_unordered <- data.table::as.data.table(m[, ind_ut, drop = FALSE] + m[, ind_lt, drop = FALSE]);
  m_unordered <- data.table::as.data.table(t(apply(m, 1, function(mm) (mm[as.vector(ind_mat)] + mm[as.vector(t(ind_mat))])[ind_ut], simplify = T)));
  m_unordered <- m_unordered[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = colnames(m_unordered)];
  
  m_unordered <- cbind(x[,rENA::find_meta_cols(x), with = FALSE], m_unordered);
  m_unordered <- reclass(x = m_unordered, c("unordered.ena.connections", "ena.connections", "ena.matrix"));
  
  m_unordered
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
'as.unordered.ordered.row.connections' <- function(x) {
  m <- as.matrix(x);
  sq_size <- sqrt(ncol(m));
  ind_mat <- matrix(seq.int(ncol(m)), nrow = sq_size, ncol = sq_size);
  ind_ut <- ind_mat[upper.tri(ind_mat, diag = FALSE)];
  ind_lt <- ind_mat[lower.tri(ind_mat, diag = FALSE)];
  
  # m_unordered <- data.table::as.data.table(m[, ind_ut, drop = FALSE] + m[, ind_lt, drop = FALSE]);
  m_unordered <- data.table::as.data.table(t(apply(m, 1, function(mm) (mm[as.vector(ind_mat)] + mm[as.vector(t(ind_mat))])[ind_ut], simplify = T)));
  m_unordered <- m_unordered[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = colnames(m_unordered)];
  
  m_unordered <- cbind(x[,rENA::find_meta_cols(x), with = FALSE], m_unordered);
  m_unordered <- reclass(x = m_unordered, c("unordered.row.connections", "row.connections", "ena.matrix"));
  
  m_unordered
}

#' Default Method for X
#'
#' This function provides the default method for handling the input `x`.
#' It prints a message indicating that the default method is being executed.
#'
#' @param x Any object that you want to apply the default method to.
#'
#' @return NULL
#'
#' @export
'as.unordered.default' <- function(x){
  cat("This is the default method. \n")
}

#' Convert to Unordered Factor
#'
#' This function is a generic method to convert an object to an unordered factor.
#' It dispatches methods based on the class of the input object.
#'
#' @param x An object to be converted to an unordered factor.
#'
#' @return An unordered factor representation of the input object.
#'
#' @export
#' @importFrom methods UseMethod
'as.unordered' <- function(x) {
  UseMethod("as.unordered")
}