# Accumulated threaded data
#
# @param ... passed along
#
# @return ENA set
accumulate_context_threads <- function(...) {
  args <- list(...);
  
  simple_window <- function(x) { (x < args$window_size) * 1 }
  if(!is.null(args$decay_function) && is.function(args$decay_function)) {
    simple_window <- args$decay_function;
  }
  no_weight <- function(x) { x };
  accumulate_contexts(
    x = args$x,
    codes = args$codes,
    return.dena.set = args$as_directed,
    return.ena.set = !args$as_directed,
    meta.data = args$meta.data,
    
    time.column = NULL,
    mask = NULL,
    time.limit = NULL,
    
    decay.function = simple_window,
    weight.by = no_weight,
    mode.column = "Thread"
  )
}

# accumulate_contexts
#
# @param x TBD
# @param codes TBD
# @param time.column TBD
# @param decay.function TBD
# @param mode.column TBD
# @param mask TBD
# @param time.limit TBD
# @param weight.by TBD
# @param meta.data TBD
# @param return.dena.set TBD
# @param return.ena.set TBD 
#
# @return ENA set
accumulate_contexts <- function(
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
  f.raw <- data.table::copy(data);
  f.raw$QEID <- seq(nrow(f.raw));
  
  use.modes = ifelse(any(is.null(f.mode)), FALSE, TRUE)
  use.meta = ifelse(any(is.null(f.meta.data)), FALSE, TRUE)
  use.mask = ifelse(is.matrix(mask), TRUE, FALSE)
  use.window = ifelse(is.null(f.time) || is.na(f.time), TRUE, FALSE)
  
  meta_cols <- x$`_function.params`$units.by;
  if(use.meta) {
    meta_cols <- unique(meta_cols, c(f.meta.data[!is.na(f.meta.data)]));
  }
  output.meta.data <- f.raw[0, c(meta_cols), with = FALSE];
  
  # initialize adjacency vector matrices
  undirected.adjacency.vectors = data.frame(matrix(0, nrow = 1, ncol = ncol(adjacency_key(f.codes))))[0,]
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
    unit.contexts <- f.contexts[[unit.index]];
    if(is.numeric(unit.contexts)) {
      unit.contexts <- list(unit.contexts)
    }
    # this.unit <- unique(units[which(apply(units, 1, paste, collapse = "::") == names(f.contexts)[unit.index]),]);
    this.unit <- attr(unit.contexts[[1]], "tma.unit");
    unit.label <- paste(as.character(this.unit), collapse = "::");
    # cat(unit.label, "\n")
    # Empty matrices for tracking unit vectors
    unit.directed.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = length(f.codes)^2)[0,]);
    unit.undirected.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = ncol(adjacency_key(f.codes))))[0,]
    # unit.meta.data <- output.meta.data[0,];

    if(is.data.frame(unit.contexts)) {
      unit.contexts <- list( unit.contexts );
    }
    
    # cat("UNIT: "); print(this.unit);
    # Each unit's contexts
    all_codes <- as.matrix(x$model$raw.input[, c(codes), with = FALSE])
    # all_codes <- Matrix(data = as.matrix(rs_set2$model$raw.input[, ..codes]), sparse = TRUE)
    res_context <- lapply(seq(unit.contexts), function(context.index) {
      this.code.matrix <- blank.code.matrix;
      this.context.rows <- unit.contexts[[context.index]];

      this.context <- x$model$raw.input[this.context.rows, ];
      this.context$CID <- seq(nrow(this.context))
      this_context_codes <- all_codes[this.context.rows,, drop = FALSE]
      # this_context_codes <- all_codes[this.context.rows,, drop = FALSE]
      this.unit.rows_tf <- this.context$QEUNIT == unit.label
      this.unit.rows <- which(this.unit.rows_tf);
      if(use.window) {
        f.time <- "CID"
      }
      
      this.time <- this.context[[c(f.time)]];
      if(use.modes) {
        these.modes <- as.factor(this.context[[c(f.mode)]]);
      }
      
      if(length(this.unit.rows) > 0) {
        response.times <- this.time[this.unit.rows];
        
        ## logical()
        grounds <- lapply(response.times, `>=`, this.time)
        
        grounds <- lapply(response.times, function(rt) {
          grnds <- rt >= this.time;
          
          if(!is.null(time.limit)) {
            grnds <- this.time >= (rt - time.limit) & grnds
          }
          grnd_index <- (cumsum(grnds) * grnds)[this.time == rt]
          attr(grnds, "tma.response.time") <- rt;
          attr(grnds, "tma.ground.index") <- grnd_index
          grnds
        });
        
        # if(!is.null(time.limit)) {
        #   grounds2 <- lapply(seq(grounds), function(x) {
        #     grounds[[x]] & this.time >= (response.times[[x]] - time.limit)
        #   })
        # }
        # cat("Grounds 2: \n"); 
        # print(grounds2)
        
        # matrix()
        grounds.codes.raw <- lapply(grounds, function(xx) this_context_codes[xx,, drop = FALSE]);
        # grounds.codes.raw2 <- lapply(grounds2, function(xx) this_context_codes[xx,, drop = FALSE]);
        
        # numeric()
        # responses.codes <- list_vec_apply(grounds.codes.raw, this.unit.rows, function(x,y) { as.numeric(x[y,,drop = FALSE]) });
        this.unit.resp.idx <- sapply(grounds, attr, which = "tma.ground.index");
        responses.codes <- list_vec_apply(grounds.codes.raw, this.unit.resp.idx, function(x,y) { as.numeric(x[y,,drop = FALSE]) });
        
        grounds.modes <- rep(NA, length(responses.codes))
        if(use.modes) {
          grounds.modes <- lapply(grounds, function(x) these.modes[x]);
        }
        ground.times <- lapply(grounds, function(x) this.time[x])
        
        # all.decays <- get.decay.modes.vec(now = response.times, event.time = ground.times, modes = grounds.modes, decay.function = f.decay);
        all.decays <- sapply(seq(ground.times), function(x) {
            subbed <- response.times[x] - ground.times[[x]]
            # sapply(seq(length(subbed)), function(y) {
            #     f.decay[[grounds.modes[[x]][y]]](subbed[y])
            # })
            # list_vec_apply_s(subbed, grounds.modes[[x]], function(xx, yy) {
            #   f.decay[[yy]](xx)
            # })
            # mapply(subbed, grounds.modes[[x]], FUN = function(xx, yy) {
            # })
            if(is.function(f.decay)) {
              f.decay(subbed)
            }
            else if (use.modes) {
              mapply(subbed, grounds.modes[[x]], FUN = function(xx, yy) {
                f.decay[[yy]](xx)
              }, SIMPLIFY = FALSE)
            }
        }, simplify = FALSE)
        # grounds.codes <- list_vec_apply(grounds.codes.raw, all.decays, fn = function(x, y) {
        #   if(length(y) == 1) {
        #     x * y
        #   }
        # 
        #   #otherwise this is a bit of fancy math that multiplies each row of the matrix
        #   #by a different scalar from a vector of scalars
        #   else {
        #     t(t(x) %*% diag(y))
        #   }
        # });
        grounds.codes = mapply(grounds.codes.raw, all.decays, FUN = function(x, y) {
          if(length(y) == 1) {
            # browser()
            x * y[[1]]
          }
          
          #otherwise this is a bit of fancy math that multiplies each row of the matrix
          #by a different scalar from a vector of scalars
          else {
            # t(t(x) %*% diag(y))
            sweep(x, MARGIN = 1, as.numeric(y), FUN = `*`)
          }
        }, SIMPLIFY = FALSE);
        
        ## Sparse Matrices
        # grounds.codes <- list_vec_apply(grounds.codes.raw, all.decays, fn = function(x, y) {
        #   if(length(y) == 1) {
        #     x * y
        #   } 
        #   
        #   #otherwise this is a bit of fancy math that multiplies each row of the matrix 
        #   #by a different scalar from a vector of scalars 
        #   else {
        #     # Matrix::t(Matrix::crossprod(x, Matrix::Diagonal(x =
        #     Matrix::t(Matrix::t(x) %*% Matrix::Diagonal(x = y))
        #   }
        # })
        
        ## numeric()
        # this.responses.in.grounds <- list_vec_apply(grounds.codes, this.unit.rows, function(x, g) { x[g, ] }) #, drop = FALSE] }) ## Sparse Matrices
        this.responses.in.grounds <- list_vec_apply(grounds.codes, this.unit.resp.idx, function(x, g) { x[g, ] }) #, drop = FALSE] }) ## Sparse Matrices
        
        ## numeric()
        # these.grounds <- lapply(grounds.codes, function(x) Matrix::colSums(x) );
        these.grounds <- lapply(grounds.codes, function(x) colSums(x) );
        
        ## numeric()
        all_only_ground_connections <- list_vec_apply(
          x = these.grounds,
          y = this.responses.in.grounds,
          fn = function(x, y) x - y
        );
        
        ## Sparse Matrices
        # all_connection_matrices_raw <- list_vec_apply(all_only_ground_connections, responses.codes, Matrix::crossprod);
        # matrix
        all_connection_matrices_raw <- list_vec_apply(all_only_ground_connections, responses.codes, function(x, y) crossprod(t(x), y));
        
        # response_matrices <- lapply(responses.codes, function(x) as.matrix(Matrix::crossprod(x)));
        response_matrices <- lapply(responses.codes, function(x) tcrossprod(x) );
        these_self_connections <- lapply(response_matrices, function(x) {
          0.5 * (x - diag(diag(x)))
        })
        # all_connection_matrices <- list_vec_apply(all_connection_matrices_raw, these_self_connections, `+`);
        all_connection_matrices <- lapply(seq(all_connection_matrices_raw), function(i) {
          all_connection_matrices_raw[[i]] + these_self_connections[[i]]
        })
        # if (f.weighting.function == 'binary') {
        #   stop ("Need to re-implement here")
        # }
        # else {
        #   unit_directed_adjacency <- Reduce(function(x, i) {
        #     x + (i * mask)
        #   }, x = all_connection_matrices, init = blank.code.matrix);
        #   print(unit_directed_adjacency)
        # }

        # dir_adj <- t(sapply(all_connection_matrices, function(x) as.vector(x) * mask)); 
        # dir_adj <- as.vector(this.connection.matrix * mask)
        # dir_mats <- Reduce(x = all_connection_matrices, f = `+`, init = blank.code.matrix, accumulate = TRUE)[-1];
        dir_mats <- as.matrix(Reduce(x = all_connection_matrices, f = `+`, init = blank.code.matrix, accumulate = FALSE));
        # print(dir_mats)
        # dir_adj <- do.call(rbind, lapply(dir_mats, as.vector))
        
        # unit.directed.adjacency.vectors <<- rbind(unit.directed.adjacency.vectors, dir_adj);
        # undir_adj <- as.undirected.vector(as.undirected.matrix(this.connection.matrix * mask))
        # undir_adj <- do.call(rbind, lapply(lapply(dir_mats, as.undirected.matrix), as.undirected.vector));
        # undirected.mat <- as.undirected.matrix(Reduce(f = function(x, i ) { x + (i * mask) }, x = all_connection_matrices, init = blank.code.matrix))
        # unit.undirected.adjacency.vectors <<- rbind(unit.undirected.adjacency.vectors, undir_adj)
        
        # data.frame(dir_adj)
        
        dim(dir_mats) <- c(1, prod(dim(dir_mats) ))
        data.frame(dir_mats)
      }
    })
    
    # browser()
    unit.directed.adjacency.vectors <- data.table::rbindlist(res_context)
    directed.adjacency.vectors  <- rbind( directed.adjacency.vectors,  colSums(unit.directed.adjacency.vectors) );

    unit.undirected.adjacency.vectors <- unit.directed.adjacency.vectors[, {
      as.list(as.undirected.vector(as.undirected.matrix(matrix(unlist(.SD), ncol = sqrt(length(.SD))))))
    }, by = seq(nrow(unit.directed.adjacency.vectors))][, -1]
    
    undirected.adjacency.vectors <- rbind( undirected.adjacency.vectors, colSums(unit.undirected.adjacency.vectors) );
    
    # output.meta.data <- rbind(output.meta.data, unique(unit.meta.data));
    # if(use.meta) {
    # }
    output.meta.data <- rbind(output.meta.data, as.list(this.unit[, c(meta_cols), with = FALSE]), use.names = FALSE)
    
    # dir_len <- nrow(directed.adjacency.vectors);
    # undir_len <- nrow(undirected.adjacency.vectors);
    # directed.adjacency.vectors[(dir_len + 1):(dir_len + nrow(unit.directed.adjacency.vectors)),] <- unit.directed.adjacency.vectors;
    # undirected.adjacency.vectors[(undir_len + 1):(dir_len + nrow(unit.undirected.adjacency.vectors)),] <- unit.undirected.adjacency.vectors;
  }
  
  #### Construct ENA Sets ----
  
  #add additional information for modeling
  directed.adjacency.vectors <- structure(
    lapply(directed.adjacency.vectors, reclass, cl = "ena.co.occurrence"), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(t(apply(matrix(c(rep(seq(f.codes), times = length(f.codes)), rep(seq(f.codes), each = length(f.codes))), ncol = 2), 2, function(x) LETTERS[x])), 2, paste, collapse = " & ")
  );
  
    # lapply(undirected.adjacency.vectors, as.ena.co.occurrence), 
  undirected.adjacency.vectors <- structure(
    lapply(undirected.adjacency.vectors, reclass, cl = "ena.co.occurrence"), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(adjacency_key(f.codes), 2, paste, collapse = " & ")
  );
  
  # if(use.meta) {
    x$meta.data <- structure(
      lapply(output.meta.data, reclass, cl = "ena.metadata"), 
      class = c("ena.matrix", "data.table", "data.frame")
    )
  # }
  
  # Standard ENA Set
  if(return.ena.set) {
    x$rotation = structure(list(
      codes = f.codes,
      adjacency.key = structure(
        adjacency_key(f.codes),
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
    
    output = x;
  }
  
  # Directed ENA Set
  else {
    dena_data = ena.set.directed(f.raw, f.units, NA, f.codes)
    dena_data$meta.data <- data.table::as.data.table(output.meta.data)
    dena_data$meta.data[, c("ENA_UNIT") := merge_columns(dena_data$meta.data, cols = x$`_function.params`$units.by)]
    for( i in colnames(dena_data$meta.data) ) {
      data.table::set(dena_data$meta.data, j = i, value = reclass(dena_data$meta.data[[i]], "ena.metadata"))
    }
    code_length <- length(dena_data$rotation$codes);
    dena_data$rotation$adjacency.key <- data.table::data.table(matrix(c(
      rep(1:code_length, each = code_length),
      rep(1:code_length, code_length)),
      byrow = TRUE, nrow = 2
    ))
    dena_data$connection.counts <- data.table::as.data.table(cbind(dena_data$meta.data, directed.adjacency.vectors))
    dena_data$model$row.connection.counts = data.table::as.data.table(cbind(dena_data$meta.data, directed.adjacency.vectors))
    
    dena_data$connection.counts = reclass(dena_data$connection.counts, c("ena.connections", "ena.matrix"))
    dena_data$model$row.connection.counts <- reclass(dena_data$model$row.connection.counts, c("row.connections", "ena.matrix"))
    
    output = dena_data;
  }
  
  output$model$contexts <- f.contexts
  
  return(output)
}