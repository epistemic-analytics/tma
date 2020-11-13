#' Accumulate Units
#'
#' @param context.object TBD
#' @param time.column if no time column is given, use model with equal time = 1 between lines in a hoo
#' @param codes TBD
#' @param decay.function TBD
#' @param mode.column TBD
#' @param mask TBD
#' @param time.limit TBD
#' @param weight.by TBD
#' @param meta.data TBD
#' @param return.ena.set TBD
#' @param return.dena.set TBD
#'
#' @return TBD
#' @export
accumulate.units = function(
  context.object, 
  time.column = NA,
  codes, 
  decay.function = NA, 
  mode.column = NA, 
  mask = NA,
  time.limit = NA,
  weight.by = sqrt,
  meta.data = NA,
  return.dena.set = FALSE,
  return.ena.set = TRUE
) {
  f.window.limit = time.limit
  f.contexts = context.object$contexts
  f.codes = codes
  f.time = time.column
  f.decay = decay.function
  f.mode = mode.column
  f.weighting.function = weight.by
  f.meta.data = meta.data
  f.units = as.data.frame(context.object$units)
  f.raw = as.data.frame(context.object$raw.data)
  
  use.modes = ifelse(any(is.na(f.mode)), FALSE, TRUE)
  use.meta = ifelse(any(is.na(f.meta.data)), FALSE, TRUE)
  use.mask = ifelse(is.matrix(mask), TRUE, FALSE)
  use.window = ifelse(any(is.na(f.time)), TRUE, FALSE)
  
  #create undirected mask if there are NAs in lower triangle of given mask
  if(use.mask) {
    if (any(is.na(mask[lower.tri(mask)]))) {
      mask[lower.tri(mask)] = mask[upper.tri(mask)]
    }
  }
  else {
    mask <- matrix(1, nrow = length(f.codes), ncol = length(f.codes));
  }
  
  # initialize metadata matrix, if needed
  # Always initialize output.meta.data, only add extra metadata beyond units when necessary
  output.meta.data = f.raw[0, unique(c(names(f.units), f.meta.data[!is.na(f.meta.data)]))]
  
  # initialize adjacency vector matrices
  undirected.adjacency.vectors = as.matrix(data.frame(matrix(NA, nrow = 1, ncol = ncol(rENA::namesToAdjacencyKey(f.codes))))[0,])
  directed.adjacency.vectors = as.matrix(data.frame(matrix(NA, nrow = 1, ncol = length(f.codes)^2)[0,]))
  colnames(directed.adjacency.vectors) = paste("V", seq(length(f.codes) ^ 2), sep = "")
  
  
  #create blank code matrix template
  n = length(f.codes)
  blank.code.matrix = matrix(0, n, n)
  colnames(blank.code.matrix) = f.codes
  rownames(blank.code.matrix) = f.codes
  #blank.code.matrix
  
  # for use with binary flag
  binarize_window = function(x) {return(ifelse(x>0,1,0))}
  
  #loop over contexts
  for (this.hoo in 1:length(f.contexts)) {
    f.contexts[[this.hoo]]$directed.adjacency = blank.code.matrix;
    f.contexts[[this.hoo]]$undirected.adjacency = vector("numeric", length(f.codes));
    unit.undirected.adjacency.vectors = as.matrix(data.frame(matrix(NA, nrow = 1, ncol = ncol(rENA::namesToAdjacencyKey(f.codes))))[0,])
    unit.directed.adjacency.vectors = as.matrix(data.frame(matrix(NA, nrow = 1, ncol = length(f.codes)^2)[0,]));
    
    
    #get data from this context
    the_contexts <- f.contexts[[this.hoo]]$context
    if ( is.list(the_contexts) && is.data.frame(the_contexts) ) {
      the_contexts <- list(the_contexts)
    }
    for ( this.context in the_contexts ) { #f.contexts[[this.hoo]]$context) {
      #initialize connection  matrix for this unit
      this.code.matrix = blank.code.matrix
      
      #get unit name
      this.unit = f.contexts[[this.hoo]]$unit
      
      # print(this.unit);
      # print(this.context);
      
      # this.context = as.data.frame(f.contexts[[this.hoo]]$context)
      this.context = as.data.frame(this.context);
      #set row numbers
      Order = seq(nrow(this.context))
      this.context = cbind(Order, this.context)
    
      #remove rows with no codes from the data
      #we don't need them as "spacers" for the rest of the data
      #because we are using time to define our connections
      # this.context = this.context[rowSums(as.matrix(this.context[,f.codes])) > 0,]
      
      Rows = seq_len(nrow(this.context))
      this.context = cbind(Rows, this.context)
      
      #find rows belonging to unit
      # browser()
      # this.unit.data = semi_join(this.context, this.unit, by = names(this.unit))
      
      ## I'm not sure if the column ordering the reason behind using dplyr here, so I've added an additional [,] after
      ## merge so the results are identical to the call to `semi_join`, if not, then we should just use the option here.
      # this.unit.data <- merge(this.context, this.unit, by = names(this.unit))
      this.unit.data <- merge(this.context, this.unit, by = names(this.unit))[, colnames(this.context)]
      
      #get meta data for this unit and add to the the meta data dataframe
      # output.meta.data = rbind(output.meta.data, get.unique.col.values(this.unit.data[,colnames(output.meta.data)]))
      output.meta.data = rbind(output.meta.data, get.unique.col.values(this.unit[,colnames(output.meta.data)]))
      
      #if there are non-zero rows for this unit, compute adjacencies
      if(nrow(this.unit.data) > 0) {
        this.unit.data <- this.unit.data[order(this.unit.data$Order), ]
    
        #get rows associated with this unit
        unit.rows = this.unit.data$Rows
      
        #get times, or if window model (ie, not times given), use rows
        if(use.window) {
          this.time = this.context$Order
        }
        else {
          this.time = this.context[, f.time]
        }
        
        #print(this.time)
        
        if(use.modes) these.modes = this.context[,f.mode]
      
        #loop on  unit (ie, response) rows
        for (response in unit.rows){
          #for each response, find earlier chats
          
          response.time = this.time[response]
          ground = which(this.time <= response.time)
          #if there is a limit on how far back in time to look, take only rows
          #that are after that limit
          if(!any(is.na(f.window.limit))) {
            ground = which(this.time[ground] >= (response.time - f.window.limit))
          }
          
          #find row numbers of the current response row WITHIN GROUND
          response.in.ground = which(ground == response)
  
          #get codes for ground and response
          response.codes = as.matrix(this.context[response, f.codes])
          ground.codes = as.matrix(this.context[ground, f.codes])
          # browser();
  
          #compute decays
          #
          #Get modes, if needed
          ground.modes = NA
          if(use.modes) {
            ground.modes = these.modes[ground]
          }
          
          #get decays -- get.decay.modes handles whether to use modes or not
          my.decays = get.decay.modes(
              now = response.time,
              event.time = this.time[ground],
              modes = ground.modes,
              decay.function = f.decay
          )
  
          #apply decays to ground
          # 
          #if there is only a single line of data, multiple by scalar
          if(length(my.decays) == 1) {
            ground.codes = ground.codes * my.decays 
          }
          
          #otherwise this is a bit of fancy math that multiplies each row of the matrix 
          #by a different scalar from a vector of scalars 
          else {            
            ground.codes = t(t(ground.codes) %*% diag(my.decays))
          }
          # browser();
          #find the ground code corresponsing the the response line
          #since the response line is in its own "window" for connections
          #and we can't assume that for its mode the decay at time_0 = 1
          this.response.in.ground = t(ground.codes[response.in.ground,])
          
          #collapse the ground codes into a vector colsums
          this.ground = t(as.matrix(.colSums(ground.codes, m = length(ground), n = length(f.codes))))
          only_ground_connections = this.ground - this.response.in.ground
  
          #compute connections
          # this.connection.matrix = crossprod(this.ground, response.codes)
          this.connection.matrix = crossprod(only_ground_connections, response.codes)
          #compute the connections from the response to itself
          # this.self.connection = crossprod(this.response.in.ground, response.codes)
          response.matrix <- crossprod(response.codes)
          # this.self.connection = 0.5*(t(response.codes)%*%response.codes - as.vector(response.codes) * diag(length(response.codes)))
          this.self.connection = 0.5*(response.matrix - diag(diag(response.matrix)))
  
          #remove self-references in response code from from the adjacency counts
          # diag(this.connection.matrix) = diag(this.connection.matrix) - diag(this.self.connection)
          this.connection.matrix = this.connection.matrix + this.self.connection
          
          #weighting function lets us account for inflation of connections
          #due to multiplication of weighted code values
          #we do the same thing in weighted ENA
          if (class(f.weighting.function) != 'function' & return.ena.set){
            if (f.weighting.function == 'binary') {

              # AJF HACK
              # In order to get TMA accumulation to agree w/ rENA accumulation, we would need to perform the weighting
              # on each window _after_ the undirected connections have been added up. This means that directed co-occurences
              # would be treated as a single connection, the same as code connections across utterances.
              # First, we need to apply the mask to the _window_'s connections
              if(use.mask) this.connection.matrix = this.connection.matrix * mask

              # upper triangle is some of directed connections
              upper_tri_indices <- upper.tri(this.connection.matrix)
              diag(this.connection.matrix) = 0
              this.connection.matrix[upper_tri_indices] <- this.connection.matrix[upper_tri_indices] + t(this.connection.matrix)[upper_tri_indices]

              #make matrix symmetrical
              this.connection.matrix[lower.tri(this.connection.matrix)] = 0
              this.code.matrix = this.code.matrix + binarize_window(this.connection.matrix);
            }
            else {
              stop('specify a valid weighting function')
            }
          }
          else {
            this.code.matrix = this.code.matrix + f.weighting.function(this.connection.matrix)
            # print(this.code.matrix);
          }
          #go back to next row for this unit
        } #done looping over rows for this unit
          
      } #end of if statement -- the above is all skipped if there are no
        #non-zero rows to loop over for the unit
      # browser()
      #for this unit...
     
      # Apply mask ------
      ### to the adjacency matrix 
      # if(use.mask) this.code.matrix = this.code.matrix * mask
    
      # Directed Adjacency ------
        # add directed adjacency matrix to the context
        f.contexts[[this.hoo]]$directed.adjacency = f.contexts[[this.hoo]]$directed.adjacency + (this.code.matrix * mask)
        
        #construct directed adjacency vector
        unit.directed.adjacency.vectors = rbind(unit.directed.adjacency.vectors, as.vector(this.code.matrix * mask))

      # Undirected Adjacency ------
        #commenting this out keeps self-connections in undirecteed matrix
        #but it will be removed for ena adjacency vectors
        #diag(this.code.matrix) = 0
        
        # turn directed into undirected
        
        #upper triangle is some of directed connections
        upper_tri_indices <- upper.tri(this.code.matrix);
        ut_matrix <- this.code.matrix * mask;
        ut_matrix[upper_tri_indices] <- ut_matrix[upper_tri_indices] + t(ut_matrix)[upper_tri_indices]
        # this.code.matrix[upper_tri_indices] <- this.code.matrix[upper_tri_indices] + t(this.code.matrix)[upper_tri_indices]
        
        #make matrix symmetrical
        ut_matrix[lower.tri(ut_matrix)] = NA
        
        #add undirected matrix to context 
        f.contexts[[this.hoo]]$undirected.adjacency = f.contexts[[this.hoo]]$undirected.adjacency + (ut_matrix * mask);
        # print( (ut_matrix * mask) );
        
        #create undirected vector
        unit.undirected.adjacency.vectors = rbind( unit.undirected.adjacency.vectors, ut_matrix[upper_tri_indices] );
    }
    
    directed.adjacency.vectors = rbind( directed.adjacency.vectors, colSums(unit.directed.adjacency.vectors) );
    undirected.adjacency.vectors = rbind( undirected.adjacency.vectors, colSums(unit.undirected.adjacency.vectors) );
    
    # go back to next context
  }
  #done looping over contexts
  
  #construct output
  # output = list()
  
  #add additional information for modeling
  directed.adjacency.vectors <- as.ena.matrix(data.table::as.data.table(directed.adjacency.vectors), "ena.connections")
  undirected.adjacency.vectors <- as.ena.matrix(data.table::as.data.table(undirected.adjacency.vectors), "ena.connections")
  for(col in seq(ncol(directed.adjacency.vectors))) {
    set(x = directed.adjacency.vectors, j = col, value = as.ena.co.occurrence(directed.adjacency.vectors[[col]]))
  }
  for(col in seq(ncol(undirected.adjacency.vectors))) {
    set(x = undirected.adjacency.vectors, j = col, value = as.ena.co.occurrence(undirected.adjacency.vectors[[col]]))
  }
  
  #### Construct ENA Sets ----
  
  output <- list()
  
  # Standard ENA Set
  if(return.ena.set) {
    enadata = structure(list(), class = c("ENAdata", "list"))
    enadata$adjacency.matrix = rENA::namesToAdjacencyKey(f.codes)
    enadata$codes = f.codes
    enadata$model = "TMA"
    enadata$raw = data.table::as.data.table(f.raw)
    enadata$units = f.units
    enadata$meta.data = data.table::as.data.table(output.meta.data)
    enadata$metadata = data.table::as.data.table(output.meta.data)
    enadata$adjacency.vectors = data.table::as.data.table(undirected.adjacency.vectors)
    enadata$accumulated.adjacency.vectors = data.table::as.data.table(undirected.adjacency.vectors)
    
    output = rENA:::ena.set(enadata) #output$for.ena)
  }
  
  # Directed ENA Set
  else {
    ENA_UNIT <- rENA::merge_columns_c(f.units, cols = colnames(f.units));
    
    dena_data = ena.set.directed(f.raw, f.units, NA, f.codes)
    dena_data$meta.data <- data.table::as.data.table(cbind(ENA_UNIT, output.meta.data))
    for( i in colnames(dena_data$meta.data) ) {
      set(dena_data$meta.data, j = i, value = rENA::as.ena.metadata(dena_data$meta.data[[i]]))
    }
    code_length <- length(dena_data$rotation$codes);
    dena_data$rotation$adjacency.key <- data.table::data.table(matrix(c(
      rep(1:code_length, each = code_length),
      rep(1:code_length, code_length)),
      byrow = TRUE, nrow = 2
    ))
    dena_data$connection.counts <- data.table::as.data.table(cbind(dena_data$meta.data, directed.adjacency.vectors))
    dena_data$connection.counts = rENA::as.ena.matrix(x = dena_data$connection.counts, "ena.connections")
    # for( i in c('ENA_UNIT', colnames(f.units)) ) {
    #   set(dena_data$connection.counts, j = i, value = rENA::as.ena.metadata(dena_data$connection.counts[[i]]))
    # }
    # dena_data$model$row.connection.counts = data.table::as.data.table(cbind( as.ena.matrix(x = directed.adjacency.vectors, "row.connections")))
    # dena_data$model$row.connection.counts = cbind(dena_data$meta.data,data.table::as.data.table(directed.adjacency.vectors));
    dena_data$model$row.connection.counts = data.table::as.data.table(cbind(dena_data$meta.data, directed.adjacency.vectors))
    dena_data$model$row.connection.counts <- rENA::as.ena.matrix(dena_data$model$row.connection.counts, "row.connections")
    
    output = dena_data;
  }
  
  output$model$contexts <- f.contexts;
  
  # browser()
  
  return(output)
}


#' Accumulate Units
#'
#' @param x TBD
#' @param codes TBD
#' @param time.column if no time column is given, use model with equal time = 1 between lines in a hoo
#' @param decay.function TBD
#' @param mode.column TBD
#' @param mask TBD
#' @param time.limit TBD
#' @param weight.by TBD
#' @param meta.data TBD
#' @param return.ena.set TBD
#' @param return.dena.set TBD
#'
#' @return TBD
#' @export
accumulate_units = function(
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
  binarize_window = function(x) {return(ifelse(x>0,1,0))}

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
  use.window = ifelse(any(is.null(f.time) | is.na(f.time)), TRUE, FALSE)
  
  meta_cols <- unique(c(f.meta.data[!is.na(f.meta.data)]));
  output.meta.data <- f.raw[0, c(meta_cols), with = FALSE];
  
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
    unit.contexts <- f.contexts[[unit.index]];
    
    # Empty matrices for tracking unit vectors
    unit.undirected.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = ncol(rENA::namesToAdjacencyKey(f.codes))))[0,]
    unit.directed.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = length(f.codes)^2)[0,]);
    unit.meta.data <- output.meta.data[0,];
    
    if(is.data.frame(unit.contexts)) {
      unit.contexts <- list( unit.contexts );
    }
    
    # print(this.unit)
    
    # Each unit's contexts
    for ( context.index in seq(unit.contexts) ) {
      this.code.matrix <- blank.code.matrix;
      this.context.rows <- unit.contexts[[context.index]];
      # print(this.context.rows)

      if( is.vector(this.context.rows) ) {
        this.context <- data.table::copy( f.raw[this.context.rows, ] );
      }
      else {
        this.context <- data.table::copy( this.context.rows );
      }
      this.context$CID <- seq( nrow(this.context) );
      
      this.unit.data <- merge(this.context, this.unit, by = names(this.unit), sort = FALSE);
      if(use.window) {
        f.time <- "QEID"
      }
      
      this.time <- this.context[[c(f.time)]];
      if(use.modes) {
        these.modes <- this.context[[c(f.mode)]];
      }
      
      bk <- data.table::copy(this.context)
      
      this.unit.rows <- bk[this.unit, , on = names(this.unit), which = TRUE, nomatch = NULL];
      
      unit.meta.data = rbind(unit.meta.data, unique(this.context[this.unit.rows, colnames(unit.meta.data), with = FALSE]))
    
      for( ur in this.unit.rows ) {
        response.time <- this.time[ur];
        ground <- which(this.time <= response.time)
        response.in.ground <- which(ground == ur);
        ground.codes = this.context[ground, c(codes), with = FALSE]
        response.codes <- ground.codes[.N, ];
        ground.codes <- as.matrix(ground.codes);
        response.codes <- as.matrix(response.codes);
        
        ground.modes <- NA;
        if(use.modes) {
          ground.modes <- these.modes[ground];
        }
        
        my.decays <- get.decay.modes(
          now = response.time,
          event.time = this.time[ground],
          modes = ground.modes,
          decay.function = f.decay
        )
        # print(my.decays);
        
        if( length(my.decays) == 1 ) {
          ground.codes <- ground.codes * my.decays;
        } 
        
        # otherwise this is a bit of fancy math that multiplies each row of the matrix 
        # by a different scalar from a vector of scalars 
        else {            
          ground.codes <- t(t(ground.codes) %*% diag(my.decays));
        }
        
        #find the ground code corresponding to the response line
        #since the response line is in its own "window" for connections
        #and we can't assume that for its mode the decay at time_0 = 1
        this.response.in.ground = ground.codes[response.in.ground,]
        
        #collapse the ground codes into a vector colsums
        this.ground = t(as.matrix(.colSums(ground.codes, m = length(ground), n = length(f.codes))))
        only_ground_connections = as.matrix(this.ground - this.response.in.ground)
        
        #compute connections
        this.connection.matrix = crossprod(only_ground_connections, response.codes)
        
        # print("Response codes: ");
        # print(response.codes);
        # print("This ground: ");
        # print(this.ground);
        # print("Only ground conns: ");
        # print(only_ground_connections);
        
        #compute the connections from the response to itself
        response.matrix <- crossprod(response.codes)
        this.self.connection = 0.5 * (response.matrix - diag(diag(response.matrix)))

        #remove self-references in response code from from the adjacency counts
        this.connection.matrix = this.connection.matrix + this.self.connection
        
        #weighting function lets us account for inflation of connections
        #due to multiplication of weighted code values
        #we do the same thing in weighted ENA
        
        # print("Connection matrix: ");
        # print(this.connection.matrix)
        if (class(f.weighting.function) != 'function' & return.ena.set){
          if (f.weighting.function == 'binary') {
            
            # AJF HACK
            # In order to get TMA accumulation to agree w/ rENA accumulation, we would need to perform the weighting
            # on each window _after_ the undirected connections have been added up. This means that directed co-occurences
            # would be treated as a single connection, the same as code connections across utterances.
            # First, we need to apply the mask to the _window_'s connections
            if(use.mask) this.connection.matrix = this.connection.matrix * mask
            
            # upper triangle is some of directed connections
            upper_tri_indices <- upper.tri(this.connection.matrix)
            diag(this.connection.matrix) = 0
            this.connection.matrix[upper_tri_indices] <- this.connection.matrix[upper_tri_indices] + t(this.connection.matrix)[upper_tri_indices]
            
            #make matrix symmetrical
            this.connection.matrix[lower.tri(this.connection.matrix)] = 0
            this.code.matrix = this.code.matrix + binarize_window(this.connection.matrix);
          }
          else {
            stop('specify a valid weighting function')
          }
        }
        else {
          this.code.matrix <- this.code.matrix + f.weighting.function(this.connection.matrix)
        }
        
        # print("Code matrix: ");
        # print(this.code.matrix)
        
        # Directed Adjacency ------
          # add directed adjacency matrix to the context
          unit.code.matrices[[names(f.contexts)[unit.index]]]$directed.adjacency <- unit.code.matrices[[names(f.contexts)[unit.index]]]$directed.adjacency + (this.code.matrix * mask)
        
          #construct directed adjacency vector
          #unit.directed.adjacency.vectors <<- rbind(unit.directed.adjacency.vectors, as.vector(this.code.matrix * mask))
          unit.directed.adjacency.vectors[nrow(unit.directed.adjacency.vectors) + 1, ] <- as.vector(this.code.matrix * mask)
      
        # Undirected Adjacency ------
          #upper triangle is some of directed connections
          upper_tri_indices <- upper.tri(this.code.matrix);
          ut_matrix <- this.code.matrix * mask;
          ut_matrix[upper_tri_indices] <- ut_matrix[upper_tri_indices] + t(ut_matrix)[upper_tri_indices]
          
          #make matrix symmetrical
          ut_matrix[lower.tri(ut_matrix)] = NA
          
          #add undirected matrix to context 
          unit.code.matrices[[names(f.contexts)[unit.index]]]$undirected.adjacency <- 
            unit.code.matrices[[names(f.contexts)[unit.index]]]$undirected.adjacency + (ut_matrix * mask);
        
          #create undirected vector
          # unit.undirected.adjacency.vectors <<- rbind( unit.undirected.adjacency.vectors, ut_matrix[upper_tri_indices] );
          # print("Ut vector: ");
          # print(ut_matrix[upper_tri_indices]);
          unit.undirected.adjacency.vectors[nrow(unit.undirected.adjacency.vectors) + 1, ] <- ut_matrix[upper_tri_indices];
      # })
      }
    }

    # print("Unit dir adj vectors: ");
    # print(unit.directed.adjacency.vectors);
    # print("Unit undir adj vectors: ");
    # print(unit.undirected.adjacency.vectors);
    
    directed.adjacency.vectors <- rbind( directed.adjacency.vectors, colSums(unit.directed.adjacency.vectors) );
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
    
    dena_data = ena.set.directed(f.raw, f.units, NA, f.codes)
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
#'
#' @examples
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
    unit.contexts <- f.contexts[[unit.index]];
    if(is.numeric(unit.contexts)) {
      unit.contexts <- list(unit.contexts)
    }
    # this.unit <- unique(units[which(apply(units, 1, paste, collapse = "::") == names(f.contexts)[unit.index]),]);
    this.unit <- attr(unit.contexts[[1]], "tma.unit");
    unit.label <- paste(this.unit, collapse = "::");
    # cat(unit.label, "\n")
    # Empty matrices for tracking unit vectors
    unit.directed.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = length(f.codes)^2)[0,]);
    unit.undirected.adjacency.vectors <- data.frame(matrix(0, nrow = 1, ncol = ncol(rENA::namesToAdjacencyKey(f.codes))))[0,]
    # unit.meta.data <- output.meta.data[0,];

    if(is.data.frame(unit.contexts)) {
      unit.contexts <- list( unit.contexts );
    }
    
    # cat("UNIT: "); print(this.unit);
    # Each unit's contexts
    all_codes <- as.matrix(x$model$raw.input[, ..codes])
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
              })
            }
        })
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
            x * y
          }
          
          #otherwise this is a bit of fancy math that multiplies each row of the matrix
          #by a different scalar from a vector of scalars
          else {
            # t(t(x) %*% diag(y))
            sweep(x, MARGIN = 1, y, FUN = `*`)
          }
        });
        
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
        these.grounds <- lapply(grounds.codes, function(x) Matrix::colSums(x) );
        
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
    lapply(directed.adjacency.vectors, rENA::as.ena.co.occurrence), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(t(apply(matrix(c(rep(seq(f.codes), times = length(f.codes)), rep(seq(f.codes), each = length(f.codes))), ncol = 2), 2, function(x) LETTERS[x])), 2, paste, collapse = " & ")
  );
  
  undirected.adjacency.vectors <- structure(
    lapply(undirected.adjacency.vectors, rENA::as.ena.co.occurrence), 
    class = c("ena.connections", "data.table", "data.frame"),
    names = apply(rENA::namesToAdjacencyKey(f.codes), 2, paste, collapse = " & ")
  );
  
  # if(use.meta) {
    x$meta.data <- structure(
      # lapply(unique(output.meta.data), rENA::as.ena.metadata),
      lapply((output.meta.data), rENA::as.ena.metadata),
      class = c("ena.matrix", "data.table", "data.frame")
    )
  # }
  
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
    
    dena_data = ena.set.directed(f.raw, f.units, NA, f.codes)
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