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
    # time.limit = NULL,
    
    decay.function = simple_window,
    weight.by = no_weight,
    mode.column = "Thread",
    context_filter = args$context_filter
  )
}

ground_response_crossprod <- function(
  x, y
) {
  UNIT = NULL;
  UNIT_LABEL = NULL;
  eff_call_env <- new.env(parent = environment(ground_effect_function));
  environment(ground_effect_function) <- eff_call_env;
  assign("UNIT", UNIT, envir = eff_call_env);
  assign("UNIT_LABEL", UNIT_LABEL, envir = eff_call_env);
  
  # all_connection_matrices_raw <- list_vec_apply(all_only_ground_connections, responses.codes, ground_effect_function);
  all_connection_matrices_raw <- list_vec_apply(x, y, ground_effect_function);
  
  response_matrices <- lapply(y, function(x) tcrossprod(x) );
  these_self_connections <- lapply(response_matrices, function(x) {
    0.5 * (x - diag(diag(x)))
  })
  
  all_connection_matrices <- lapply(seq(all_connection_matrices_raw), function(i) {
    all_connection_matrices_raw[[i]] + these_self_connections[[i]]
  })
  
  dir_vecs <- t(sapply(all_connection_matrices, function(x) { dim(x) <- c(1, nrow(x)^2); (x) }));
  
  acc_call_env <- new.env(parent = environment(accumulate_unit_vectors_by));
  environment(accumulate_unit_vectors_by) <- acc_call_env;
  assign("UNIT", UNIT, envir = acc_call_env);
  assign("UNIT_LABEL", UNIT_LABEL, envir = acc_call_env);
  dir_vecs <- accumulate_unit_vectors_by(dir_vecs);
  
  return(dir_vecs)
}

#' accumulate_contexts
#'
#' @param x TBD
#' @param codes TBD
#' @param ... TBD
#' @param time.column TBD
#' @param decay.function TBD
#' @param mode.column TBD
#' @param mask TBD
#' @param weight.by TBD
#' @param meta.data TBD
#' @param return.dena.set TBD
#' @param return.ena.set TBD 
#' @param accumulate_unit_vectors_by TBD 
#'
#' @return ENA set
#' @export
accumulate_contexts <- function(
  x,
  codes,
  decay.function = decay(simple_window, window_size = 4),
  time.column = NULL,
  mode.column = NULL,
  mask = NULL,
  weight.by = sqrt,
  norm.by = `_sphere_norm`,
  meta.data = NULL,
  return.dena.set = FALSE,
  return.ena.set = TRUE,
  context_filter = NULL,
  summarize_ground_using = colSums,
  calculate_adj_vectors_using = ground_response_crossprod,
  ground_effect_function = function(x, y) crossprod(t(x), y),
  accumulate_unit_vectors_by = colSums
) {
  data <- x$model$raw.input;
  f.contexts <- x$model$contexts; #context.object #$contexts
  units <- x$`_function.params`$units; 
  
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
    
    this.unit <- attr(unit.contexts[[1]], "tma.unit");
    if(is.null(this.unit)) {
      this.unit <- attr(unit.contexts, "tma.unit");
    }
    unit.label <- paste(as.character(this.unit), collapse = "::");
    # Empty matrices for tracking unit vectors
    unit.directed.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = length(f.codes)^2)[0,]);
    unit.undirected.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = ncol(adjacency_key(f.codes))))[0,]

    if(is.data.frame(unit.contexts)) {
      unit.contexts <- list( unit.contexts );
    }
    
    # Each unit's contexts
    all_codes <- as.matrix(x$model$raw.input[, c(codes), with = FALSE])
    res_context <- lapply(seq(unit.contexts), function(context.index) {
      this.code.matrix <- blank.code.matrix;
      this.context.rows <- unit.contexts[[context.index]];
      if(is.numeric(this.context.rows)) {
        this.context <- x$model$raw.input[this.context.rows, ];
      }
      else {
        this.context <- x$model$raw.input[this.context.rows, , on = c("QEID")];
      }
      this.context$CID <- seq(nrow(this.context))
      # browser()
      # this_context_codes <- all_codes[this.context.rows,, drop = FALSE]
      this_context_codes <- all_codes[this.context.rows$QEID,, drop = FALSE]
      this.unit.rows_tf <- this.context$QEUNIT == unit.label
      if(!is.null(context_filter)) {
        this.unit.rows_tf <- do.call(context_filter, list(unit = unit.label, context = this.context))  
      }
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
        
        grounds <- lapply(this.unit.rows, function(ur) {
          grounds <- vector(length = nrow(this.context))
          grounds[seq_len(ur)] <- TRUE
          attr(grounds, "tma.response.index") <- ur
          grounds
        })
        
        grounds.codes.raw <- lapply(grounds, function(xx) this_context_codes[xx,, drop = FALSE]);
        
        this.unit.resp.idx <- sapply(grounds, attr, which = "tma.response.index");
        responses.codes <- list_vec_apply(grounds.codes.raw, this.unit.resp.idx, function(x,y) { as.numeric(x[y,,drop = FALSE]) });
        
        grounds.modes <- rep(NA, length(responses.codes))
        if(use.modes) {
          grounds.modes <- lapply(grounds, function(x) these.modes[x]);
        }
        ground.times <- lapply(grounds, function(x) this.time[x])
        
        all.decays <- sapply(seq(grounds), function(wh) {
          call_env <- new.env(parent = environment(f.decay));
          RESPONSE_INDEX <- attr(grounds[[wh]], which = "tma.response.index")
          # RESPONSE <- this.context[RESPONSE_INDEX, ]
          ROWS <- grounds[[wh]]
          # CONTEXT <- this.context[this_rows, ]
          # RESPONSE <- CONTEXT[.N]
          assign("FULL_CONTEXT", this.context, envir = call_env)
          # assign("CONTEXT", CONTEXT, envir = call_env)
          assign("RESPONSE_INDEX", RESPONSE_INDEX, envir = call_env)
          assign("ROWS", ROWS, envir = call_env)
          environment(f.decay) <- call_env;
          
          subbed <- as.numeric(response.times[wh] - ground.times[[wh]])
          if(is.function(f.decay)) {
            f.decay(subbed)
            # CONTEXT[ , {
            #   assign("GROUND_ROW", .SD, envir = call_env);
            #   f.decay(subbed[.GRP])
            # }, by = which(grounds[[wh]])][[2]];
          }
          else if (use.modes) {
            mapply(FUN = function(xx, yy, INDEX) {
              do.call(xx, args = list(x = yy))
            }, f.decay[grounds.modes[[wh]]], subbed, which(grounds[[wh]]))
          }
        }, simplify = FALSE)
        
        grounds.codes = mapply(grounds.codes.raw, all.decays, FUN = function(x, y) {
          
          if(length(y) == 1) {
            x * y[[1]]
          }
          
          #otherwise this is a bit of fancy math that multiplies each row of the matrix
          #by a different scalar from a vector of scalars
          else {
            # browser()
            # sweep(x, MARGIN = 1, (y), FUN = `*`)
            x * y
          }
        }, SIMPLIFY = FALSE);
        
        this.responses.in.grounds <- list_vec_apply(grounds.codes, this.unit.resp.idx, function(x, g) { x[g, ] })
        these.grounds <- lapply(grounds.codes, function(x) summarize_ground_using(x) );
        
        all_only_ground_connections <- list_vec_apply(
          x = these.grounds,
          y = this.responses.in.grounds,
          fn = function(x, y) x - y
        );
        
        ##
        adj_call_env <- new.env(parent = environment(calculate_adj_vectors_using));
        environment(calculate_adj_vectors_using) <- adj_call_env;
        assign("UNIT", this.unit, envir = adj_call_env);
        assign("UNIT_LABEL", unit.label, envir = adj_call_env);
        assign("ground_effect_function", ground_effect_function, envir = adj_call_env);
        assign("accumulate_unit_vectors_by", accumulate_unit_vectors_by, envir = adj_call_env);
        dir_vecs <- calculate_adj_vectors_using(all_only_ground_connections, responses.codes)
        
        dim(dir_vecs) <- c( 1, length(dir_vecs) );
        
        data.frame(dir_vecs);
      }
    })
    
    unit.directed.adjacency.vectors <- data.table::rbindlist(res_context)
    directed.adjacency.vectors  <- rbind( directed.adjacency.vectors,  colSums(unit.directed.adjacency.vectors) );

    if(nrow(unit.directed.adjacency.vectors) > 0) {
      unit.undirected.adjacency.vectors <- unit.directed.adjacency.vectors[, {
        as.list(
          as.undirected.vector(as.undirected.matrix(matrix(unlist(.SD), ncol = sqrt(length(.SD)))))
        )
      }, by = seq(nrow(unit.directed.adjacency.vectors))][, -1]
    }
    
    undirected.adjacency.vectors <- rbind( undirected.adjacency.vectors, colSums(unit.undirected.adjacency.vectors) );
    
    output.meta.data <- rbind(output.meta.data, as.list(this.unit[, c(meta_cols), with = FALSE]), use.names = FALSE)
  }
  
  #### Construct ENA Sets ----
  
  #add additional information for modeling
  directed.adjacency.vectors <- structure(
    lapply(directed.adjacency.vectors, reclass, cl = "ena.co.occurrence"), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(t(apply(matrix(c(rep(seq(f.codes), times = length(f.codes)), rep(seq(f.codes), each = length(f.codes))), ncol = 2), 2, function(x) f.codes[x])), 2, paste, collapse = " & ")
  );
  
  # lapply(undirected.adjacency.vectors, as.ena.co.occurrence), 
  undirected.adjacency.vectors <- structure(
    lapply(undirected.adjacency.vectors, reclass, cl = "ena.co.occurrence"), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(adjacency_key(f.codes), 2, paste, collapse = " & ")
  );
  
  x$meta.data <- structure(
    lapply(output.meta.data, reclass, cl = "ena.metadata"), 
    class = c("ena.matrix", "data.table", "data.frame")
  );
  
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
    # browser()
    
    if(!is.null(norm.by)) {
      x$line.weights <- norm.by(x = as.matrix(undirected.adjacency.vectors))
    }
    
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