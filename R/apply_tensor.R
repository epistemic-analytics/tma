#' Apply windowing and weighting to context data for network accumulation
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `apply_tensor_old` is a deprecated alias for the tensor-based accumulation
#' function.  Use [accumulate()] for the full TMA accumulation pipeline.
#'
#' @param tensor NumericVector. Flat multi-dimensional context_tensor array.
#' @param dims IntegerVector. Dimensions of the original context_tensor array.
#' @param dims_sender Integer vector. 0-based indices of sender dimensions.
#' @param dims_receiver Integer vector. 0-based indices of receiver dimensions.
#' @param dims_mode Integer vector. 0-based indices of mode dimensions.
#' @param context_matrix NumericMatrix. Context lookup matrix (rows = context lines, cols = factors).
#' @param unit_rows Integer vector. 0-based response-row indices for this unit.
#' @param codes Numeric matrix. Code matrix (nrow = context lines, ncol = number of codes).
#' @param times Numeric vector. Timestamp per context row.
#' @param ordered Logical. TRUE = directed; FALSE = undirected upper-triangle.
#' @return List with \code{row_connection_counts} and \code{connection_counts}.
#' @export
apply_tensor_old <- function(tensor, dims, dims_sender, dims_receiver, dims_mode,
                              context_matrix, unit_rows, codes, times,
                              ordered = TRUE) {
  lq_apply_tensor(tensor, dims, dims_sender, dims_receiver, dims_mode,
                  context_matrix, unit_rows, codes, times, ordered)
}
