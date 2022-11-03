#' Title
#'
#' @param x TBD
#' @param code_cols TBD
#' @param decay_function TBD
#' @param time_col TBD
#' @param bind_meta TBD
#'
#' @return TBD
#' @export
accumulate <- function (
  x,
  code_cols,
  decay_function = function(x) { rep_len(1, length(x)) },
  time_col = NULL,
  bind_meta = TRUE
) {
  stopifnot(is(x, "ena.set"));
  stopifnot(is.function(decay_function));
  
  code_col_indices <- NULL;
  if(is.character(code_cols)) {
    code_col_indices <- which(names(x$model$contexts[[1]]) %in% code_cols);
  }
  else {
    code_col_indices <- code_cols;
  }
  
  if(is.null(time_col)) {
    time_col <- -1;
  }
  
  ENA_UNIT <- NULL;
  meta_cols <- x$`_function.params`$units.by;
  meta_data <- data.table::rbindlist(lapply(x$model$contexts, attr, "tma.unit"));
  meta_data[, ENA_UNIT := do.call(paste, c(.SD, sep="::")), .SDcols = meta_cols];
  x$meta.data <- structure(
    lapply(meta_data, reclass, cl = "ena.metadata"), 
    class = c("ena.matrix", "data.table", "data.frame")
  );
  
  accumulated <- accumulate_networks(x = x, code_cols = (code_col_indices - 1), decay_function = decay_function, time_col = time_col);
  accumulated.dt <- data.table::as.data.table(accumulated$networks);
  x$connection.counts <- structure(
    lapply(accumulated.dt, reclass, cl = "ena.co.occurrence"), 
    class = c("ena.connections", "ena.matrix", "data.table", "data.frame")
    # ,model = x
  );
  if(bind_meta == TRUE) {
    x$connection.counts <- cbind(x$meta.data, x$connection.counts);
    class(x$connection.counts) <- c("ena.connections", "ena.matrix", "data.table", "data.frame");
  }
  
  x$model$row.connection.counts <- accumulated$networks_by_row;
  
  code_length <- length(code_cols);
  code_names <- names(x$model$contexts[[1]])[code_col_indices];
  code_factor <- factor(code_names, levels = code_names);
  # adjacency.key <- data.table::data.table(
  adjacency.key <- list(
    V1 = code_factor[rep(1:code_length, each = code_length)],
    V2 = code_factor[rep(1:code_length, code_length)]
  );
  adjacency.key <- reclass(adjacency.key, "adjacency.key");
  adjacency.key <- data.table::as.data.table(t(as.numeric(adjacency.key)));
  
  x$rotation <- structure(
    list(
      codes = code_names,
      nodes = NULL,
      adjacency.key = adjacency.key
    ),
    class = c("networks.rotation", "list")
    # ,model = x
  );

  # connection.counts <- data.table::as.data.table(cbind(meta_data, accumulated));
  # row.connection.counts <- data.table::as.data.table(cbind(meta_data, accumulated));
  # 
  # connection.counts <- reclass(connection.counts, c("networks.connections", "network.matrix"));
  # row.connection.counts <- reclass(row.connection.counts, c("row.connections", "network.matrix"));

  class(x) <- c("ena.directed.set", class(x));
  x;
}