
#' Title
#'
#' @param x 
#' @param codes 
#' @param time.column 
#' @param decay.function 
#' @param mode.column 
#' @param mask 
#' @param time.limit 
#' @param weight.by 
#' @param meta.data 
#' @param return.dena.set 
#' @param return.ena.set 
#'
#' @return
#' @export
accumulate_contexts3 <- function(
  x,
  codes,
  time.column = NULL,
  decay.function = NULL,
  mode.column = NULL,
  mask = NULL,
  time.limit = NULL,
  weight.by = sqrt,
  meta.data = NULL,
  return.dena.set = FALSE,
  return.ena.set = TRUE
) {
  # meta_cols <- unique(c(meta.data[!is.na(meta.data)]));
  output.meta.data <- x$model$raw.input[0, c(meta.data), with = FALSE];
  code_rows <- as.matrix(x$model$raw.input[, c(codes), with = FALSE]) 
  adj_vecs_by_context <- rapply(x$model$contexts, f = function(ctx) {
    rows <- x$model$raw.input[QEID %in% ctx, .SD, keyby = "QEID"];
    unit <- attr(x = ctx, which = "tma.unit");
    unit_label <- paste(unit, collapse = "::")
    # raw_conn_vecs <- rows[as.list(unit), {
    raw_conn_vecs <- rows[QEUNIT == unit_label, {
      now <- .BY[["QEID"]]
      # browser( expr = { length(now) == 0 })
      # now <- .SD[["QEID"]]
      if(length(now) > 0 && !is.na(now)) {
        times_in <- rows[["QEID"]] <= now;
        times <- rows[["QEID"]][times_in];
        # print(times)
        # print(rows)
        #print(rows[QEID %in% times, ])
        
        these_rows <- code_rows[times,, drop = FALSE];
        #print(these_rows)
        if(nrow(these_rows) > 0) {        
          funs <- decay.function[rows[["Context"]][times_in]]
          # this.ground.decayed <- t(mapply(FUN = function(z, y) {
          #   these_rows * z(now - y)
          # }, funs, times, USE.NAMES = FALSE) )
          # this.ground.decayed <- these_rows * sapply(seq(nrow(these_rows)), function(r) { funs[[r]](now - times[r]) });
          # decays <- get.decay.modes(now = now, event.time = times, modes = rows[["Context"]], decay.function = decay.function);
          n <- sample(seq(length((times))), size = 1);
          decays <-  c(rep(0, n), rep(1, length(times)-n))
          this.ground.decayed <- these_rows * decays;
          # print(this.ground.decayed)
          response.codes <- tail(this.ground.decayed, 1);
          only_ground_connections <- colSums(this.ground.decayed) - response.codes;
          this.connection.matrix <- crossprod(only_ground_connections, response.codes);
          response.matrix <- crossprod(response.codes)
          this.connection.matrix <- this.connection.matrix + (0.5 * (response.matrix - diag(diag(response.matrix))))
          
          ret <- as.vector(this.connection.matrix)
          attributes(ret) <- attributes(ctx)
          # print(ret)
          as.list(ret)
        }
      }
    # }, on = names(unit), by = "QEID"][, -1]
    }, by = "QEID"][, -1]
    
    if(nrow(raw_conn_vecs) > 0) {
      output.meta.data <<- rbind(output.meta.data, unit[c(meta.data)], use.names = FALSE);
      # browser()
      dir_adj <- raw_conn_vecs[, lapply(.SD, cumsum)] #i, .SDcols = LETTERS[1:9]]
      # unit.directed.adjacency.vectors <- rbind(unit.directed.adjacency.vectors, dir_adj, use.names = FALSE);
      
      undir_adj <- dir_adj[, {
        mat <- matrix(data = unlist(.SD), nrow = sqrt(ncol(.SD)), ncol = sqrt(ncol(.SD))) 
        as.list(as.undirected.vector(as.undirected.matrix(mat)))
      }, by = seq(nrow(dir_adj))][, -1]
      # unit.undirected.adjacency.vectors <- rbind(unit.undirected.adjacency.vectors, undir_adj[, -1], use.names = FALSE)
      
      # browser()
      list(directed = dir_adj, undirected = undir_adj);
      # dir_adj
    }
  }, classes = "ANY", how = "r");
  
  dir_undirs <- sapply(c("directed", "undirected"), FUN = function(wh) {
      dirs <- sapply((names(adj_vecs_by_context)), FUN = function(y) {
          dirs2 <- lapply(seq(adj_vecs_by_context[[y]]), FUN = function(n) {
              adj_vecs_by_context[[y]][[n]][[wh]];
          });
          data.table::rbindlist(dirs2)[, lapply(.SD, sum)]
      });
      #dirs_t <- data.table::as.data.table(cbind(ENA_UNIT = rownames(dirs_t), dirs_t))
      dirs_t <- data.table::as.data.table(t(dirs))
      rownames(dirs_t) <- NULL
      dirs_t
  })
  
  directed.adjacency.vectors <- dir_undirs$directed;
  undirected.adjacency.vectors <- dir_undirs$undirected;
  directed.adjacency.vectors <- structure(
    lapply(directed.adjacency.vectors, rENA::as.ena.co.occurrence), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(t(apply(matrix(c(rep(seq(codes), times = length(codes)), rep(seq(codes), each = length(codes))), ncol = 2), 2, function(x) LETTERS[x])), 2, paste, collapse = " & ")
  );
    
  undirected.adjacency.vectors <- structure(
    lapply(undirected.adjacency.vectors, rENA::as.ena.co.occurrence), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(rENA::namesToAdjacencyKey(codes), 2, paste, collapse = " & ")
  );
  
  x$meta.data <- structure(
    # lapply(unique(output.meta.data), rENA::as.ena.metadata),
    lapply(unique(output.meta.data), rENA::as.ena.metadata),
    class = c("ena.matrix", "data.table", "data.frame")
  )
  
  # Standard ENA Set
  if(return.ena.set) {
    x$rotation = structure(list(
      codes = codes,
      adjacency.key = structure(
        rENA::namesToAdjacencyKey(codes),
        dimnames = list( NULL, names(undirected.adjacency.vectors))
      )
    ), class = c("ena.rotation.set", "list"));
    
    x$connection.counts <- data.table::copy(undirected.adjacency.vectors);
    x$connection.counts <- structure(cbind(x$meta.data, x$connection.counts), class = class(undirected.adjacency.vectors));
    
    # TODO: This needs to be updated to reflect the accumulation for each row in the data
    # enadata$accumulated.adjacency.vectors = data.table::as.data.table(undirected.adjacency.vectors)
    x$model$row.connection.counts <- structure(
      data.table::copy(x$connection.counts),
      class = c("row.connections", "ena.matrix", "data.table", "data.frame")
    );
    
    output = x; #rENA:::ena.set(enadata);
  }
  
  # Directed ENA Set
  else {
    ENA_UNIT <- rENA::merge_columns_c(f.units, cols = colnames(f.units));
    
    dena_data = ena.set.directed(data, f.units, NA, codes)
    dena_data$meta.data <- data.table::as.data.table(cbind(ENA_UNIT, output.meta.data))
    for( i in colnames(dena_data$meta.data) ) {
      data.table::set(dena_data$meta.data, j = i, value = rENA::as.ena.metadata(dena_data$meta.data[[i]]))
    }
    code_length <- length(dena_data$rotation$codes);
    dena_data$rotation$adjacency.key <- data.table::data.table(matrix(c(
      rep(1:code_length, each = code_length),
      rep(1:code_length, code_length)),
      byrow = TRUE, nrow = 2
    ))
    dena_data$connection.counts <- data.table::as.data.table(cbind(dena_data$meta.data, directed.adjacency.vectors))
    dena_data$connection.counts = rENA::as.ena.matrix(x = dena_data$connection.counts, "ena.connections")
    dena_data$model$row.connection.counts = data.table::as.data.table(cbind(dena_data$meta.data, directed.adjacency.vectors))
    dena_data$model$row.connection.counts <- rENA::as.ena.matrix(dena_data$model$row.connection.counts, "row.connections")
    
    output = dena_data;
  }
  
  output$model$contexts <- x$model$contexts
  
  return(output)
}