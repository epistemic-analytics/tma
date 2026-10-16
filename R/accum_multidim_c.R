#' Accumulate Connections from a Multidimensional Array and Context Model
#'
#' This function processes a context model and a multidimensional array of window/weight parameters to compute connection counts for each unit of analysis.
#' It applies the context model to the array, using sender, receiver, and mode columns (as defined in the array attributes), and accumulates co-occurrence or adjacency matrices for each unit. The result is a set of connection counts and row-level connection matrices, suitable for network analysis (e.g., ENA/ONA).
#'
#' @param context_model A context model object (as produced by `tma::contexts`) containing contexts for each unit of analysis.
#' @param codes Character vector of code names to use for constructing adjacency matrices.
#' @param tensor A multidimensional array (see `context_tensor`) containing window and weight values for each sender/receiver/mode combination. Defaults to an array generated from the context model's raw input.
#' @param time_column Character string giving the name of the time column in the context model. If NULL, uses the default context column ID.
#' @param ordered Logical; if TRUE (default), computes ordered adjacency matrices (ONA); if FALSE, computes unordered (ENA-style) matrices.
#' @param binary Logical; if TRUE (default), binarizes each line's connection counts before they are summed per unit (unordered only; ordered counts are always summed raw). Ignored when `weight_by` is given.
#' @param weight_by Weight model applied to each line's connection counts before they are summed into the unit network (= rENA's `weight.by`): `"binary"` (unordered: each positive count becomes 1; ordered: raw directed counts), `"product"` (the raw, non-binarized counts), `"sqrt"`, or `"log1p"` (alias `"log"`). For unordered networks the weight is applied after each line is folded to the upper triangle; for ordered networks, to each directed cell. Defaults to `"binary"` when `binary = TRUE`, otherwise `"product"`. `model$row.connection.counts` always holds the raw, unweighted line counts.
#' @param misses Logical; if TRUE, also accumulate missed co-occurrences: for each response line, the (window-weighted) ground count of code i when code j is absent from the response, with no response self-term. Misses follow the connection counts through the same steps: ordered models keep the directed layout (ground i -> response j); unordered models fold each line to the upper triangle (i & j = i -> j + j -> i, self-misses dropped), and `weight_by` is applied per line. Defaults to FALSE.
#'
#' @return The input `context_model` with additional fields:
#'   \item{connection.counts}{A data.table of accumulated connection counts for each unit.}
#'   \item{model$row.connection.counts}{A data.table of row-level connection matrices for each unit.}
#'   \item{self.connection.counts}{A data.table of each unit's self-connections, one column per code: each line's ground count of the code times its response count (the ordered network's diagonal, which unordered networks fold away), weighted per line by `weight_by` and summed per unit. Kept for ordered and unordered models; not used in the model.}
#'   \item{model$row.self.connection.counts}{A data.table of raw, unweighted per-line self-connections.}
#'   \item{miss.counts}{With `misses = TRUE`: a data.table of accumulated missed co-occurrences for each unit.}
#'   \item{self.miss.counts, model$row.self.miss.counts}{With `misses = TRUE`: self-misses (the code in the ground, absent from the response), per unit and per line, as for self-connections.}
#'   \item{model$row.miss.counts}{With `misses = TRUE`: a data.table of raw, unweighted row-level missed co-occurrences.}
#'   \item{meta.data}{A data.table of metadata columns for each unit.}
#'   The class of the returned object is updated to reflect the type of accumulation (ordered or unordered).
#'
#' @details
#' This function is used to perform accumulation of network connections for each unit, based on the context model and tensor parameters. It supports both ordered and unordered accumulation, and returns results suitable for further network analysis or visualization.
#'
#' @export
accumulate <- function(
  context_model,
  codes,
  tensor = context_tensor(context_model$model$raw.input),
  time_column = NULL,
  ordered = FALSE,
  binary = TRUE,
  weight_by = NULL,
  misses = FALSE
) {

  if (is.null(weight_by)) {
    weight_by <- if (isTRUE(binary)) "binary" else "product"
  }
  if (!is.character(weight_by) || length(weight_by) != 1) {
    stop("`weight_by` must be one of 'binary', 'product', 'sqrt', 'log1p'.")
  }

  send_cols = attr(tensor, "sender_cols");
  recv_cols = attr(tensor, "receiver_cols");
  mode_cols = attr(tensor, "mode_column");

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
  
  mode_dim_1 <- dim(tensor)[1] == 1 && attr(tensor, "mode_column") == ATTR_NAMES$CONTEXT_ID;

  # Cache tensor attributes outside the loop
  tensor_dim <- attr(tensor, "dim")
  tensor_sender_inds <- attr(tensor, "sender_inds") - 1
  tensor_receiver_inds <- attr(tensor, "receiver_inds") - 1
  tensor_mode_inds <- attr(tensor, "mode_inds") - 1

  # Levels along each tensor axis, in the order context_tensor() laid them out
  # (sorted unique values across the whole dataset), one entry per column of
  # `cols_to_encode`. A column with no tensor axis (the context-id placeholder
  # rENA passes as the mode column) gets NULL and its values pass through.
  cols_to_encode <- c(send_cols, recv_cols, mode_cols)
  tensor_levels  <- dimnames(tensor)
  axis_levels    <- c(
    lapply(seq_along(send_cols), function(i) tensor_levels[[paste0("sender_", i)]]),
    lapply(seq_along(recv_cols), function(i) tensor_levels[[paste0("receiver_", i)]]),
    lapply(seq_along(mode_cols), function(i) tensor_levels[["modes"]])
  )

  # Position of each row's value along its tensor axis (1-based). Looked up in
  # the tensor's own levels -- never re-encoded per context -- so a level maps
  # to the same slice of the tensor whichever levels a context happens to
  # contain, for character-valued and numeric columns alike.
  context_lookup <- function(ctx_rows) {
    lookup <- vapply(seq_along(cols_to_encode), function(i) {
      vals <- ctx_rows[[cols_to_encode[i]]]
      lv   <- axis_levels[[i]]
      if (is.null(lv)) return(as.numeric(vals))
      pos <- match(as.character(vals), lv)
      if (anyNA(pos)) {
        stop("Values in column '", cols_to_encode[i], "' are not levels of the ",
             "tensor: ", paste(unique(vals[is.na(pos)]), collapse = ", "),
             ". Build the tensor from the same data as the contexts.")
      }
      as.numeric(pos)
    }, numeric(nrow(ctx_rows)));
    matrix(lookup, nrow = nrow(ctx_rows), ncol = length(cols_to_encode))
  }

  miss_rows <- list();
  result_cpp <- lapply(context_model$model$contexts, function(ctx) {
    ctx_fun <- function(ctx_o) {
      ctx__ <- data.table::copy(ctx_o);
      unit_context_lookup <- context_lookup(ctx__)

      unit_rows <- attr(ctx__, "tma.unit_rows")
      this_unit <- attr(ctx__, "tma.unit")
      unit_to_last <- seq.int(last(unit_rows))
      ctx_unit_to_last <- ctx__[unit_to_last, ]
      res2 <- apply_tensor(
        tensor,
        tensor_dim,
        tensor_sender_inds,
        tensor_receiver_inds,
        tensor_mode_inds,
        unit_context_lookup[unit_to_last, , drop = FALSE] - 1,
        unit_rows - 1,
        as.matrix(ctx_unit_to_last[ , ..codes]),
        ctx_unit_to_last[[time_column]],
        misses = isTRUE(misses)
      )

      rcc <- data.table::as.data.table(res2$row_connection_counts);
      unit_row_meta <- ctx_o[unit_rows, meta_cols, with = FALSE];
      # rcc <- cbind(unit_row_meta, rcc)
      # browser()
      rcc[, names(unit_row_meta) := unit_row_meta]
      data.table::setcolorder(rcc, c(names(unit_row_meta), setdiff(names(rcc), names(unit_row_meta))))

      if (isTRUE(misses)) {
        rmc <- data.table::as.data.table(res2$row_miss_counts);
        rmc[, names(unit_row_meta) := unit_row_meta]
        data.table::setcolorder(rmc, c(names(unit_row_meta), setdiff(names(rmc), names(unit_row_meta))))
        miss_rows[[length(miss_rows) + 1]] <<- rmc;
      }
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
  });
  
  row_connection_counts <- rbindlist(result_cpp);
  # colnames(row_connection_counts) <- c(meta_cols, adj_vector_names_full);
  
  # browser()
  # row_connection_counts[, lapply(seq.int(nrow(.SD)), function(col_ind) {
  #   col <- .SD[[col_ind]];
  #   new_class_nm <- ifelse(col_ind <= length(meta_cols), "ena.metadata", "ena.co.occurrence");
  #   reclass(col, new_class_nm)
  # }), .SDcols = seq.int(ncol(row_connection_counts))];
  # browser()
  row_connection_counts <- reclass_columns(row_connection_counts, rep(c("ena.metadata","ena.co.occurrence"), c(length(meta_cols), length(adj_vector_names_full))))
  data.table::setorderv(row_connection_counts, "QEID");
  row_connection_counts <- reclass(x = row_connection_counts, c("ordered.row.connections", "row.connections", "ena.matrix"));
  colnames(row_connection_counts) <- c(meta_cols, adj_vector_names_full);
  context_model$model$row.connection.counts <- row_connection_counts;
  
  # Unit networks: libqe's aggregate_row_connections finalises each line's raw
  # directed counts (fold for unordered, then the weight model) and sums the
  # lines per unit -- the same per-line stage rENA applies weight.by.
  # Column classes match the previous aggregation: ordered sums kept the
  # ena.co.occurrence class, unordered colSums returned plain numerics.
  n_codes <- length(codes);
  unit_vector <- function(rows, out_names) {
    v <- as.list(aggregate_row_connections(as.matrix(rows), n_codes, isTRUE(ordered), weight_by));
    names(v) <- out_names;
    if (isTRUE(ordered)) lapply(v, reclass, "ena.co.occurrence") else v
  };

  cc_type <- NULL;
  if(isTRUE(ordered)) {
    class(context_model) <- c("ena.ordered.set", class(context_model));
    connection_counts <- row_connection_counts[, unit_vector(.SD, adj_vector_names_full), by = units_by, .SDcols = adj_vector_names_full];
    connection_counts <- reclass(x = connection_counts, c("ordered.ena.connections", "ena.connections", "ena.matrix"));

    cc_type <- "ordered.ena.connections";
  }
  else {
    class(context_model) <- c("ena.unordered.set", class(context_model));

    connection_counts <- row_connection_counts[, unit_vector(.SD, adj_vector_names), by = "QEUNIT", .SDcols = adj_vector_names_full];
    context_model$model$row.connection.counts <- as.unordered(context_model$model$row.connection.counts);

    cc_type <- "unordered.ena.connections";
  }
  
  if (isTRUE(misses)) {
    row_miss_counts <- data.table::rbindlist(miss_rows);
    row_miss_counts <- reclass_columns(row_miss_counts, rep(c("ena.metadata","ena.co.occurrence"), c(length(meta_cols), length(adj_vector_names_full))))
    data.table::setorderv(row_miss_counts, "QEID");
    row_miss_counts <- reclass(x = row_miss_counts, c("ordered.row.connections", "row.connections", "ena.matrix"));
    colnames(row_miss_counts) <- c(meta_cols, adj_vector_names_full);
    context_model$model$row.miss.counts <- row_miss_counts;

    # Same aggregation as the connection counts above (fold + weight per line).
    if (isTRUE(ordered)) {
      miss_counts <- row_miss_counts[, unit_vector(.SD, adj_vector_names_full), by = units_by, .SDcols = adj_vector_names_full];
    }
    else {
      miss_counts <- row_miss_counts[, unit_vector(.SD, adj_vector_names), by = "QEUNIT", .SDcols = adj_vector_names_full];
      context_model$model$row.miss.counts <- as.unordered(context_model$model$row.miss.counts);
    }
  }

  # Self-connections: each line's ground i x response i (the directed rows'
  # diagonal, which unordered networks drop when they fold), weighted per line
  # and summed per unit by libtma, like the connection counts. Read from the
  # directed rows before any fold; the same for misses (ground i present,
  # response i absent). Grouped as the connection counts are, so rows line up
  # with cc_meta.
  unit_by <- if (isTRUE(ordered)) units_by else "QEUNIT";
  self_vector <- function(rows) {
    v <- as.list(aggregate_self_connections(as.matrix(rows), n_codes, isTRUE(ordered), weight_by));
    names(v) <- codes;
    lapply(v, reclass, "ena.co.occurrence")
  };
  row_self <- function(rows) {
    raw <- finalize_self_connections(as.matrix(rows[, adj_vector_names_full, with = FALSE]),
                                     n_codes, TRUE, "product");
    colnames(raw) <- codes;
    out <- cbind(rows[, meta_cols, with = FALSE], data.table::as.data.table(raw));
    out <- reclass_columns(out, rep(c("ena.metadata", "ena.co.occurrence"), c(length(meta_cols), n_codes)));
    reclass(x = out, c("row.self.connections", "ena.matrix"))
  };
  unit_self <- function(rows) {
    self_counts <- rows[, self_vector(.SD), by = unit_by, .SDcols = adj_vector_names_full];
    self_counts <- cbind(cc_meta, self_counts[, codes, with = FALSE]);
    reclass(self_counts, c("self.connections", "ena.matrix"))
  };

  cc_meta <- data.table::rbindlist(lapply(result_cpp, function(rc) rc[1, c("QEUNIT", units_by), with = FALSE]));
  cc_meta$ENA_UNIT <- context_model$model$unit.labels;
  cc_meta <- cc_meta[, lapply(.SD, reclass, "ena.metadata"), .SDcols = colnames(cc_meta)];
  
  connection_counts <- cbind(cc_meta, connection_counts)
  connection_counts <- reclass(connection_counts, c(cc_type, "ena.connections", "ena.matrix"));
  context_model$connection.counts <- connection_counts;

  context_model$self.connection.counts <- unit_self(row_connection_counts);
  context_model$model$row.self.connection.counts <- row_self(row_connection_counts);

  if (isTRUE(misses)) {
    miss_counts <- cbind(cc_meta, miss_counts);
    context_model$miss.counts <- reclass(miss_counts, c(cc_type, "ena.connections", "ena.matrix"));
    context_model$self.miss.counts <- unit_self(row_miss_counts);
    context_model$model$row.self.miss.counts <- row_self(row_miss_counts);
  }
  
  context_model$meta.data <- context_model$connection.counts[, sapply(context_model$connection.counts, is, "ena.metadata"), with = FALSE];
  class(context_model$meta.data) = c("ena.matrix", class(context_model$meta.data));

  # 5a: persist the windowing config so the accumulated object is self-describing
  # (recreate-the-model intent) and consumable by tma.conversations2, which needs
  # the resolved per-modality window/weight tensor to reproduce the exact windows.
  # See design/dataview-flexible-window.md.
  context_model$`_function.params`$tensor      <- tensor;
  context_model$`_function.params`$mode_column <- mode_cols;
  context_model$`_function.params`$codes       <- codes;
  context_model$`_function.params`$time_column <- time_column;
  context_model$`_function.params`$ordered     <- ordered;
  context_model$`_function.params`$binary      <- binary;
  context_model$`_function.params`$weight_by   <- weight_by;
  context_model$`_function.params`$misses      <- isTRUE(misses);

  return(context_model);
}
