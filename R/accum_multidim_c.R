#' Process Multidimensional Array with Context Model
#'
#' This function applies a context model to a multidimensional array, utilizing sender, receiver, and mode columns derived from the input array's attributes.
#'
#' @param context_model A context model object containing contexts to be applied to the data.
#' @param multidim_arr A multidimensional array that holds the data to be processed.
#' @param time_column A string representing the name of the column containing time data in the context model.
#' @param codes A vector of codes used to index relevant information in the context model.
#' @param ordered A logical value indicating whether the results should be ordered (default is TRUE).
#'
#' @return A list of results obtained from applying the context model to each context in the multidimensional array.
accum_multidim_c <- function(context_model, multidim_arr, time_column, codes, ordered = TRUE) {
  send_cols = attr(multidim_arr, "sender_cols");
  recv_cols = attr(multidim_arr, "receiver_cols");
  mode_cols = attr(multidim_arr, "mode_column");
  
  result_cpp <- lapply(context_model$model$contexts, function(ctx) {
    unit_context_lookup <- as.matrix(ctx[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(send_cols, recv_cols, mode_cols)]);
    unit_rows <- attr(ctx, "tma.unit_rows")
    unit_to_last <- seq.int(last(unit_rows));
    res <- apply_windows_d(
      multidim_arr,
      attr(multidim_arr, "dim"),
      attr(multidim_arr, "sender_inds") - 1,
      attr(multidim_arr, "receiver_inds") - 1,
      attr(multidim_arr, "mode_inds") - 1,
      unit_context_lookup[unit_to_last,, drop = FALSE] - 1,
      unit_rows - 1,
      as.matrix(ctx[unit_to_last, ..codes]),
      ctx[unit_to_last,][[time_column]]
    );
    
    res
  })

  context_model$rotation <- structure(list(
    codes = codes,
    adjacency.key = adjacency_key(codes, !ordered)
  ), class = c("ena.rotation.set", "list"));
  adj_vector_names <- colnames(context_model$rotation$adjacency.key);
  
  connection_indices <- if(isTRUE(ordered)) {
    seq.int(length(codes) ^ 2)
  } else { 
    which(upper.tri(matrix(nrow = length(codes), ncol = length(codes))))}
  
  meta_cols <- context_model$`_function.params`$units.by;
  connection_counts <- data.table::rbindlist(lapply(names(context_model$model$contexts), function(ctx) {
    ctx_model <- context_model$model$contexts[[ctx]]
    ctx_unit <- attr(ctx_model, "tma.unit")
    ctx_adj <- result_cpp[[ctx]]$connection_counts[, connection_indices];
    ctx_adj_dt <- as.data.table(matrix(ctx_adj, byrow = TRUE, nrow = 1, dimnames = list(NULL, adj_vector_names)))
  }));
  connection_counts <- connection_counts[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = adj_vector_names];
  connection_counts$ENA_UNIT <- context_model$model$unit.labels;
  connection_counts <- cbind(connection_counts, data.table::rbindlist(lapply(context_model$model$contexts, attr, "tma.unit")));
  
  for(jj in c("ENA_UNIT", meta_cols)) {
    data.table::set(connection_counts, j = jj, value = reclass(connection_counts[[jj]], "ena.metadata"));
  }
  data.table::setcolorder(connection_counts, c("ENA_UNIT", meta_cols, adj_vector_names));
  
  context_model$connection.counts <- connection_counts;
  
  class(context_model$connection.counts) <- c("ena.connections", "ena.matrix", "data.table", "data.frame");
  
  context_model$meta.data <- context_model$connection.counts[, sapply(context_model$connection.counts, is, "ena.metadata"), with = FALSE];
  class(context_model$meta.data) = c("ena.matrix", class(context_model$meta.data));
  
  row_connection_counts <- lapply(names(context_model$model$contexts), function(ctx) {
    ctx_model <- context_model$model$contexts[[ctx]]
    ctx_unit <- attr(ctx_model, "tma.unit")
    ctx_adj <- result_cpp[[ctx]]$row_connection_counts[, connection_indices];
    ctx_adj_dt <- as.data.table(ctx_adj);
    colnames(ctx_adj_dt) <- adj_vector_names;
    ctx_adj_dt$ENA_UNIT = ctx;
    class(ctx_adj_dt) <- c("row.connections", "ena.matrix", "data.table", "data.frame");
    ctx_adj_dt
  });
  context_model$model$row.connection.counts <- row_connection_counts;
  
  return(context_model);
}
