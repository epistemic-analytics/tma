#' Process Multidimensional Array with Context Model
#'
#' This function applies a context model to a multidimensional array, utilizing sender, receiver, and mode columns derived from the input array's attributes.
#'
#' @param context_model A context model object containing contexts to be applied to the data.
#' @param multidim_arr A multidimensional array that holds the data to be processed.
#' @param time_column A string representing the name of the column containing time data in the context model.
#' @param codes A vector of codes used to index relevant information in the context model.
#' @param ordered A logical value indicating whether the results should be ordered (default is TRUE).
#' @param binary A logical value indicating whether the connection should be binary (default is TRUE).
#'
#' @return A list of results obtained from applying the context model to each context in the multidimensional array.
#' @export
accumulate <- function(context_model, multidim_arr, codes, time_column = NULL, ordered = TRUE, binary = TRUE) {
  send_cols = attr(multidim_arr, "sender_cols");
  recv_cols = attr(multidim_arr, "receiver_cols");
  mode_cols = attr(multidim_arr, "mode_column");
  
  if(is.null(time_column)) {
    time_column <- ATTR_NAMES$CONTEXT_COL_ID;
  }
  
  
  context_model$rotation <- structure(list(
    codes = codes,
    adjacency.key = adjacency_key(codes, !ordered)
  ), class = c("ena.rotation.set", "list"));
  adj_vector_names <- colnames(context_model$rotation$adjacency.key);
  adj_vector_names_full <- colnames(adjacency_key(codes, FALSE));
  
  ind_mat <- matrix(nrow = length(codes), ncol = length(codes));
  units_by <- context_model$`_function.params`$units.by;
  meta_cols <- c("QEID", "QEUNIT", units_by);
  
  result_cpp <- lapply(context_model$model$contexts, function(ctx) {
    ctx_fun <- function(ctx__) {
      unit_context_lookup <- as.matrix(ctx__[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(send_cols, recv_cols, mode_cols)]);
      unit_rows <- attr(ctx__, "tma.unit_rows");
      this_unit <- attr(ctx__, "tma.unit");
      
      unit_to_last <- seq.int(last(unit_rows));
      res2 <- apply_windows_d(
        multidim_arr,
        attr(multidim_arr, "dim"),
        attr(multidim_arr, "sender_inds") - 1,
        attr(multidim_arr, "receiver_inds") - 1,
        attr(multidim_arr, "mode_inds") - 1,
        unit_context_lookup[unit_to_last,, drop = FALSE] - 1,
        unit_rows - 1,
        as.matrix(ctx__[unit_to_last, ..codes]),
        ctx__[unit_to_last,][[time_column]]
      );
      
      rcc <- data.table::as.data.table(res2$row_connection_counts);
      colnames(rcc) <- adj_vector_names_full;
      rcc <- rcc[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = adj_vector_names_full];
      
      unit_row_meta <- ctx__[unit_rows, meta_cols, with = FALSE];
      unit_row_meta <- unit_row_meta[, lapply(.SD, reclass, "ena.metadata"), .SDcols = meta_cols];
      rcc <- cbind(unit_row_meta, rcc);
      rcc <- reclass(x = rcc, c("ordered.row.connections", "row.connections", "ena.matrix"));
      
      cc <- data.table::as.data.table(res2$connection_counts);
      colnames(cc) <- adj_vector_names_full;
      cc <- cc[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = adj_vector_names_full];
      cc <- cbind(this_unit, cc);
      cc <- reclass(x = cc, c("ordered.ena.connections", "ena.connections", "ena.matrix"));
      
      rcc
    }
    
    if(is.data.frame(ctx)) {
      res <- ctx_fun(ctx);
    }
    else {
      results <- lapply(ctx, ctx_fun);
      
      res <- data.table::rbindlist(results);
      res <- reclass(x = res, c("ordered.row.connections", "row.connections", "ena.matrix"));
    }
    
    return(res);
  })
  
  row_connection_counts <- rbindlist(result_cpp);
  data.table::setorderv(row_connection_counts, "QEID");
  row_connection_counts <- reclass(x = row_connection_counts, c("ordered.row.connections", "row.connections", "ena.matrix"));
  context_model$model$row.connection.counts <- row_connection_counts;
  
  cc_type <- NULL
  if(isTRUE(ordered)) {
    class(context_model) <- c("ena.ordered.set", class(context_model));
    
    # connection_counts <- data.table::as.data.table(do.call(rbind, lapply(result_cpp, colSums.ena.matrix, binary = FALSE)));
    connection_counts <- data.table::as.data.table(do.call(rbind, lapply(result_cpp, function(rcpp) {
      colSums(rENA:::as.matrix.ena.matrix(rcpp))  
    })));
    connection_counts <- reclass(x = connection_counts, c("ordered.ena.connections", "ena.connections", "ena.matrix"));
    connection_counts <- connection_counts[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = colnames(connection_counts)];
    
    cc_type <- "ordered.ena.connections";
  }
  else {
    class(context_model) <- c("ena.unordered.set", class(context_model));
    
    context_model$model$row.connection.counts <- as.unordered(context_model$model$row.connection.counts);
    
    connection_counts <- context_model$model$row.connection.counts[, lapply(.SD, colSums.ena.matrix, binary = binary), by = "QEUNIT", .SDcols = adj_vector_names];
    connection_counts <- connection_counts[, lapply(.SD, reclass, "ena.co.occurrence"), .SDcols = adj_vector_names];
    cc_type <- "unordered.ena.connections"
  }
  
  cc_meta <- data.table::rbindlist(lapply(result_cpp, function(rc) rc[1, c("QEUNIT", units_by), with = FALSE]));
  cc_meta$ENA_UNIT <- context_model$model$unit.labels;
  cc_meta <- cc_meta[, lapply(.SD, reclass, "ena.metadata"), .SDcols = colnames(cc_meta)];
  
  connection_counts <- cbind(cc_meta, connection_counts)
  connection_counts <- reclass(connection_counts, c(cc_type, "ena.connections", "ena.matrix"));
  context_model$connection.counts <- connection_counts;
  
  context_model$meta.data <- context_model$connection.counts[, sapply(context_model$connection.counts, is, "ena.metadata"), with = FALSE];
  class(context_model$meta.data) = c("ena.matrix", class(context_model$meta.data));
  
  return(context_model);
}
