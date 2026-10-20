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
#' @param misses Logical; if TRUE, also accumulate missed co-occurrences: for each response line, the (window-weighted) ground count of code i when code j is absent from the response, with no response self-term. Misses follow the connection counts through the same steps: ordered models keep the directed layout (ground i -> response j); unordered models fold each line to the upper triangle (i & j = i -> j + j -> i, self-misses dropped), and `weight_by` is applied per line. Defaults to TRUE; set FALSE to skip them (less memory for many codes or long data).
#' @param self_connections Logical, default FALSE. Unordered networks only: also include each code's self-connection (ground i x response i, weighted per line like the other columns) as a column of `connection.counts` (and of `miss.counts`, the self-misses), so the model's line weights, rotation and nodes include them. Columns then run A & A, A & B, B & B, A & C, ... (the upper triangle with the diagonal; see the adjacency key). Ordered networks always include the diagonal, so it has no effect there. `self.connection.counts` is kept separately either way.
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
  misses = TRUE,
  self_connections = FALSE
) {
  # Unordered only: ordered networks always carry the diagonal.
  diagonal <- isTRUE(self_connections) && !isTRUE(ordered)


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
    adjacency.key = adjacency_key(codes, !ordered, diagonal = diagonal)
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

  # One libtma call per context (accumulate_unit_networks): it folds unordered
  # rows, applies the weight model per line, sums the lines and pulls out the
  # self-connection diagonal, for hits and (with misses) missed co-occurrences.
  # R only stacks the per-line rows into tables and adds up each unit's
  # per-context vectors (unit vectors are additive over lines).
  n_codes <- length(codes);
  result_cpp <- lapply(context_model$model$contexts, function(ctx) {
    ctx_fun <- function(ctx_o) {
      ctx__ <- data.table::copy(ctx_o);
      unit_context_lookup <- context_lookup(ctx__)

      unit_rows <- attr(ctx__, "tma.unit_rows")
      unit_to_last <- seq.int(last(unit_rows))
      ctx_unit_to_last <- ctx__[unit_to_last, ]
      res <- accumulate_unit_networks(
        tensor,
        tensor_dim,
        tensor_sender_inds,
        tensor_receiver_inds,
        tensor_mode_inds,
        unit_context_lookup[unit_to_last, , drop = FALSE] - 1,
        unit_rows - 1,
        as.matrix(ctx_unit_to_last[ , ..codes]),
        ctx_unit_to_last[[time_column]],
        ordered = isTRUE(ordered),
        weight = weight_by,
        misses = isTRUE(misses),
        diagonal = diagonal
      )
      res$meta <- ctx_o[unit_rows, meta_cols, with = FALSE];
      res
    }

    if (is.data.frame(ctx)) list(ctx_fun(ctx)) else lapply(ctx, ctx_fun)
  });
  parts <- unlist(result_cpp, recursive = FALSE);

  conn_names <- if (isTRUE(ordered)) adj_vector_names_full else adj_vector_names;

  # Per-line rows: meta columns + one column per connection (or code), in QEID
  # order, as the row tables have always been laid out.
  row_table <- function(field, cols, table_class) {
    rows <- data.table::rbindlist(lapply(parts, function(pt) {
      m <- pt[[field]];
      dt <- data.table::as.data.table(matrix(m, nrow = nrow(m), dimnames = list(NULL, cols)));
      cbind(pt$meta, dt)
    }));
    rows <- reclass_columns(rows, rep(c("ena.metadata", "ena.co.occurrence"), c(length(meta_cols), length(cols))));
    data.table::setorderv(rows, "QEID");
    reclass(x = rows, table_class)
  };

  # Unit vectors: each unit's per-context vectors summed, units in the order
  # they first appear by QEID.
  unit_of <- vapply(parts, function(pt) as.character(pt$meta$QEUNIT[1]), character(1));
  first_qeid <- tapply(vapply(parts, function(pt) min(pt$meta$QEID), numeric(1)), unit_of, min);
  unit_order <- names(sort(first_qeid));
  unit_matrix <- function(field, cols) {
    m <- rowsum(do.call(rbind, lapply(parts, `[[`, field)), unit_of, reorder = FALSE);
    m <- m[unit_order, , drop = FALSE];
    colnames(m) <- cols;
    m
  };
  unit_first_meta <- function(by_cols) {
    first <- data.table::rbindlist(lapply(parts, `[[`, "meta"));
    data.table::setorderv(first, "QEID");
    first <- first[!duplicated(first$QEUNIT), ];
    first <- first[match(unit_order, as.character(first$QEUNIT)), by_cols, with = FALSE];
    first[, lapply(.SD, reclass, "ena.metadata"), .SDcols = by_cols]
  };

  row_connection_counts <- row_table("row_connection_counts", conn_names,
    if (isTRUE(ordered)) c("ordered.row.connections", "row.connections", "ena.matrix")
    else c("unordered.row.connections", "row.connections", "ena.matrix"));
  context_model$model$row.connection.counts <- row_connection_counts;

  # Column classes match the earlier aggregation: ordered unit sums keep the
  # ena.co.occurrence class, unordered ones are plain numerics; ordered tables
  # lead with the units_by columns, unordered with QEUNIT.
  unit_table <- function(field) {
    m <- unit_matrix(field, conn_names);
    vals <- lapply(seq_len(ncol(m)), function(j) {
      v <- unname(m[, j]);
      if (isTRUE(ordered)) reclass(v, "ena.co.occurrence") else v
    });
    names(vals) <- conn_names;
    cbind(unit_first_meta(if (isTRUE(ordered)) units_by else "QEUNIT"), data.table::as.data.table(vals))
  };

  cc_type <- NULL;
  if(isTRUE(ordered)) {
    class(context_model) <- c("ena.ordered.set", class(context_model));
    connection_counts <- reclass(x = unit_table("connection_counts"), c("ordered.ena.connections", "ena.connections", "ena.matrix"));
    cc_type <- "ordered.ena.connections";
  }
  else {
    class(context_model) <- c("ena.unordered.set", class(context_model));
    connection_counts <- unit_table("connection_counts");
    cc_type <- "unordered.ena.connections";
  }

  cc_meta <- data.table::rbindlist(lapply(result_cpp, function(rc) rc[[1]]$meta[1, c("QEUNIT", units_by), with = FALSE]));
  cc_meta$ENA_UNIT <- context_model$model$unit.labels;
  cc_meta <- cc_meta[, lapply(.SD, reclass, "ena.metadata"), .SDcols = colnames(cc_meta)];

  connection_counts <- cbind(cc_meta, connection_counts)
  connection_counts <- reclass(connection_counts, c(cc_type, "ena.connections", "ena.matrix"));
  context_model$connection.counts <- connection_counts;

  # Self-connections: each line's ground i x response i (the ordered diagonal,
  # which unordered networks fold away), one column per code.
  self_table <- function(field) {
    m <- unit_matrix(field, codes);
    vals <- lapply(seq_len(ncol(m)), function(j) reclass(unname(m[, j]), "ena.co.occurrence"));
    names(vals) <- codes;
    reclass(cbind(cc_meta, data.table::as.data.table(vals)), c("self.connections", "ena.matrix"))
  };
  context_model$self.connection.counts <- self_table("self_connection_counts");
  context_model$model$row.self.connection.counts <- row_table("row_self_connection_counts", codes, c("row.self.connections", "ena.matrix"));

  if (isTRUE(misses)) {
    context_model$model$row.miss.counts <- row_table("row_miss_counts", conn_names,
      if (isTRUE(ordered)) c("ordered.row.connections", "row.connections", "ena.matrix")
      else c("unordered.row.connections", "row.connections", "ena.matrix"));
    miss_counts <- if (isTRUE(ordered))
      reclass(x = unit_table("miss_counts"), c("ordered.ena.connections", "ena.connections", "ena.matrix"))
    else unit_table("miss_counts");
    context_model$miss.counts <- reclass(cbind(cc_meta, miss_counts), c(cc_type, "ena.connections", "ena.matrix"));
    context_model$self.miss.counts <- self_table("self_miss_counts");
    context_model$model$row.self.miss.counts <- row_table("row_self_miss_counts", codes, c("row.self.connections", "ena.matrix"));
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
  context_model$`_function.params`$self_connections <- diagonal;

  return(context_model);
}
