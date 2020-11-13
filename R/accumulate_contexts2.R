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
accumulate_contexts2 <- function(
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
  data <- x$model$raw.input;
  f.contexts <- x$model$contexts; #context.object #$contexts
  units <- x$`_function.params`$units; 
  
  f.window.limit = time.limit
  f.codes = codes
  f.time = time.column
  f.decay = decay.function
  f.mode = mode.column
  f.weighting.function = weight.by
  f.meta.data = meta.data
  f.units = as.data.frame(names(f.contexts))
  
  use.modes = ifelse(any(is.null(f.mode)), FALSE, TRUE)
  use.meta = ifelse(any(is.null(f.meta.data)), FALSE, TRUE)
  use.mask = ifelse(is.matrix(mask), TRUE, FALSE)
  use.window = ifelse(any(is.null(f.time) | is.na(f.time)), TRUE, FALSE)
  
  meta_cols <- unique(c(f.meta.data[!is.na(f.meta.data)]));
  output.meta.data <- data[0, c(meta_cols), with = FALSE];
  
  # initialize adjacency vector matrices
  undirected.adjacency.vectors = data.frame(matrix(0, nrow = 1, ncol = ncol(rENA::namesToAdjacencyKey(f.codes))))[0,]
  directed.adjacency.vectors = data.frame(matrix(0, nrow = 1, ncol = length(f.codes)^2)[0,]);
  
  #create undirected mask if there are NAs in lower triangle of given mask
  if( use.mask ) {
    if ( any(is.na(mask[lower.tri(mask)])) ) {
      mask[lower.tri(mask)] = mask[upper.tri(mask)]
    }
  }
  else {
    mask <- matrix(1, nrow = length(f.codes), ncol = length(f.codes));
  }
  
  n = length(codes)
  blank.code.matrix = matrix(0, nrow = n, ncol = n, dimnames = list(codes, codes))
  unit.code.matrices <- structure(rep(list(list(
    directed.adjacency = blank.code.matrix,
    undirected.adjacency = structure(rep(0, ncol(blank.code.matrix)))
    
  )), length(names(f.contexts))), names = names(f.contexts))
  for( unit.index in seq(f.contexts)) {
    this.unit <- unique(units[which(apply(units, 1, paste, collapse = "::") == names(f.contexts)[unit.index]),]);
    # cat("UNIT: "); print(this.unit);
    
    unit.contexts <- f.contexts[[unit.index]];
    
    # Empty matrices for tracking unit vectors
    unit.directed.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = length(f.codes)^2)[0,]);
    unit.undirected.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = ncol(rENA::namesToAdjacencyKey(f.codes))))[0,]
    unit.meta.data <- output.meta.data[0,];

    if(is.data.frame(unit.contexts)) {
      unit.contexts <- list( unit.contexts );
    }
    
    # Each unit's contexts
    for ( context.index in seq(unit.contexts) ) {
      this.code.matrix <- blank.code.matrix;
      this.context.rows <- unit.contexts[[context.index]];
      this.context <- data[this.context.rows, .SD, keyby = "QEID"];
      this.context$CID <- seq( nrow(this.context) )
      data.table::setkeyv(this.context, c("CID"))
      # cat("Context: \n"); print(this.context);
      
      this.unit.data <- merge(this.context, this.unit, by = names(this.unit), sort = FALSE);
      # this.unit.data <- this.context[this.unit, ]
      if(use.window) {
        f.time <- "QEID"
      }
      
      this.time <- this.context[[c(f.time)]];
      if(use.modes) {
        these.modes <- this.context[[c(f.mode)]];
      }
      # bk <- data.table::copy(this.context)
      # this.unit.rows <- this.context[this.unit, , on = names(this.unit), which = TRUE, nomatch = NULL];
      this.unit.rows <- this.context[this.unit, CID, on = names(this.unit), nomatch = NULL];
      
      unit.meta.data = rbind(unit.meta.data, unique(this.context[this.unit.rows, colnames(unit.meta.data), with = FALSE]))
      
      grounds2 <- this.context[this.unit, {
        list(CID = this.context[this.context[[f.time]] <= .BY, , which = TRUE])
      }, by = c("QEID"), on = names(this.unit)]
      # data.table::setkeyv(grounds2, c("CID"))
      
      if(nrow(grounds2) > 0) {
        raw_conn_vecs <- merge(grounds2, this.context, by = "CID")[, {
          rows <- .SD;
          time <- rows$CID;
          now <- tail(time, 1);
          this.ground.decayed <- t(mapply(FUN = function(x, y) {
            as.matrix(rows[y, c(codes), with = FALSE]) * x(now - time[y])
          }, f.decay[rows[[f.mode]]], seq(nrow(rows)), USE.NAMES = FALSE))
          
          response.codes <- tail(this.ground.decayed, 1);
          only_ground_connections <- colSums(this.ground.decayed) - response.codes;
          this.connection.matrix <- crossprod(only_ground_connections, response.codes);
          response.matrix <- crossprod(response.codes)
          this.connection.matrix <- this.connection.matrix + (0.5 * (response.matrix - diag(diag(response.matrix))))
          
          as.list(as.vector(f.weighting.function(this.connection.matrix) * mask))
        }, by = "QEID.x"][, -1]

        # browser( expr = { this.unit$UserName == "User3" })
        dir_adj <- raw_conn_vecs[, lapply(.SD, cumsum)] #i, .SDcols = LETTERS[1:9]]
        unit.directed.adjacency.vectors <- rbind(unit.directed.adjacency.vectors, dir_adj, use.names = FALSE);
        
        # undir_adj <- as.undirected.vector(as.undirected.matrix(this.connection.matrix * mask))
        # undir_adj <- do.call(rbind, lapply(lapply(dir_mats, as.undirected.matrix), as.undirected.vector));
        undir_adj <- dir_adj[, {
          mat <- matrix(data = unlist(.SD), nrow = sqrt(ncol(.SD)), ncol = sqrt(ncol(.SD))) 
          as.list(as.undirected.vector(as.undirected.matrix(mat)))
        }, by = seq(1:nrow(dir_adj))]
        # undirected.mat <- as.undirected.matrix(Reduce(f = function(x, i ) { x + (i * mask) }, x = all_connection_matrices, init = blank.code.matrix))
        # browser()
        unit.undirected.adjacency.vectors <- rbind(unit.undirected.adjacency.vectors, undir_adj[, -1], use.names = FALSE)
      }
    }
    
    directed.adjacency.vectors   <- rbind( directed.adjacency.vectors,   colSums(unit.directed.adjacency.vectors) );
    undirected.adjacency.vectors <- rbind( undirected.adjacency.vectors, colSums(unit.undirected.adjacency.vectors) );
    output.meta.data <- rbind(output.meta.data, unique(unit.meta.data));
  }
  
  #### Construct ENA Sets ----
  
  #add additional information for modeling
  directed.adjacency.vectors <- structure(
    lapply(directed.adjacency.vectors, rENA::as.ena.co.occurrence), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(t(apply(matrix(c(rep(seq(f.codes), times = length(f.codes)), rep(seq(f.codes), each = length(f.codes))), ncol = 2), 2, function(x) LETTERS[x])), 2, paste, collapse = " & ")
  );
  
  undirected.adjacency.vectors <- structure(
    lapply(undirected.adjacency.vectors, rENA::as.ena.co.occurrence), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(rENA::namesToAdjacencyKey(f.codes), 2, paste, collapse = " & ")
  );
  
  x$meta.data <- structure(
    # lapply(unique(output.meta.data), rENA::as.ena.metadata),
    lapply((output.meta.data), rENA::as.ena.metadata),
    class = c("ena.matrix", "data.table", "data.frame")
  )
  
  # Standard ENA Set
  if(return.ena.set) {
    x$rotation = structure(list(
      codes = f.codes,
      adjacency.key = structure(
        rENA::namesToAdjacencyKey(f.codes),
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
    
    dena_data = ena.set.directed(data, f.units, NA, f.codes)
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
  
  output$model$contexts <- f.contexts
  
  return(output)
}