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
#'
#' @return TBD
#' @export
accumulate.units = function(context.object, 
                            time.column = NA,  
                            codes, 
                            decay.function = default.decay.function, 
                            mode.column = NA, 
                            mask = NA,
                            time.limit = NA,
                            weight.by = sqrt,
                            meta.data = NA,
                            return.ena.set = TRUE
                            ){
  f.window.limit = time.limit
  f.contexts = context.object$contexts
  f.codes = codes
  f.time = time.column
  f.decay = decay.function
  f.mode = mode.column
  f.weighting.function = weight.by
  f.meta.data = meta.data
  f.units = context.object$units
  f.raw = context.object$raw.data
  
  use.modes = ifelse(any(is.na(f.mode)), FALSE, TRUE)
  use.meta = ifelse(any(is.na(f.meta.data)), FALSE, TRUE)
  use.mask = ifelse(is.matrix(mask), TRUE, FALSE)
  use.window = ifelse(any(is.na(f.time)), TRUE, FALSE)
  
  #create undirected mask if there are NAs in lower triangle of given mask
  if (any(is.na(mask[lower.tri(mask)]))) {mask[lower.tri(mask)] = mask[upper.tri(mask)]}
  
  #initialize metadata matrix, if needed
  # Always initialize output.meta.data, only add extra metadata beyond units when necessary
  output.meta.data = f.raw[0, unique(c(names(f.units),f.meta.data[!is.na(f.meta.data)]))]
  
  #initialize adjacency vector matrices
  undirected.adjacency.vectors = as.matrix(
    data.frame(matrix(NA, nrow = 1, ncol = ncol(rENA:::namesToAdjacencyKey(f.codes))))[0,]
  )
  directed.adjacency.vectors = as.matrix(
    data.frame(matrix(NA, nrow = 1, ncol = length(f.codes)^2)[0,])
  )
  
  
  #create blank code matrix template
  n = length(f.codes)
  blank.code.matrix = matrix(0, n, n)
  colnames(blank.code.matrix) = f.codes
  rownames(blank.code.matrix) = f.codes
  #blank.code.matrix
  
  #loop over contexts
  for (this.hoo in 1:length(f.contexts)) {
    #initialize connection  matrix for this unit
    this.code.matrix = blank.code.matrix
    
    #get data from this context
    this.context = f.contexts[[this.hoo]]$context
    
  
    #set row numbers
    Order = 1:nrow(this.context)
    this.context = cbind(Order, this.context)
  
    #remove rows with no codes from the data
    #we don't need them as "spacers" for the rest of the data
    #because we are using time to define our connections
    this.context = this.context[rowSums(as.matrix(this.context[,f.codes])) > 0,]
    
    Rows = 1:nrow(this.context)
    this.context = cbind(Rows, this.context)
    
    #get unit name
    this.unit = f.contexts[[this.hoo]]$unit
    
    #find rows belonging to unit
    # browser()
    # this.unit.data = semi_join(this.context, this.unit, by = names(this.unit))
    
    ## I'm not sure if the column ordering the reason behind using dplyr here, so I've added an additional [,] after
    ## merge so the results are identical to the call to `semi_join`, if not, then we should just use the option here.
    # this.unit.data <- merge(this.context, this.unit, by = names(this.unit))
    this.unit.data <- merge(this.context, this.unit, by = names(this.unit))[, colnames(this.context)]
    
    #get meta data for this unit and add to the the meta data dataframe
    output.meta.data = rbind(output.meta.data, get.unique.col.values(this.unit.data[,colnames(output.meta.data)]))
    
    #if there are non-zero rows for this unit, compute adjacencies
    if(nrow(this.unit.data) > 0) {
  
        #get rows associated with this unit
        unit.rows = this.unit.data$Rows
      
        #get times, or if window model (ie, not times given), use rows
        if(use.window) {
          this.time = this.context$Order
        } else {
          this.time = this.context[,f.time]
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
          
  
          #compute decays
          #
          #Get modes, if needed
          ground.modes = NA
          if(use.modes) ground.modes = these.modes[ground]
  
          #get decays -- get.decay.modes handles whether to use modes or not
          my.decays = get.decay.modes(
              now = response.time,
              event.time = this.time[ground],
              modes = ground.modes,
              decay.function = f.decay
          )
  
          #apply decays to ground
          
          #if there is only a single line of data, multiple by scalar
          if(length(my.decays) == 1) {
            ground.codes = ground.codes * my.decays 
          } else {
            
          #otherwise this is a bit of fancy math that multiplies each row of the matrix 
          #by a different scalar from a vector of scalars 
            ground.codes = t(t(ground.codes) %*% diag(my.decays))
          }
          
          #find the ground code corresponsing the the response line
          #since the response line is in its own "window" for connections
          #and we can't assume that for its mode the decay at time_0 = 1
          this.response.in.ground = t(ground.codes[response.in.ground,])
  
          #collapse the ground codes into a vector colsums
          this.ground = t(as.matrix(.colSums(ground.codes, m = length(ground), n = length(f.codes))))
  
          #compute connections
          this.connection.matrix = crossprod(this.ground, response.codes)
          
          #compute the connections from the response to itself
          this.self.connection = crossprod(this.response.in.ground, response.codes)
  
          #remove self-references in response code from from the adjacency counts
          diag(this.connection.matrix) = diag(this.connection.matrix) - diag(this.self.connection)
          
          #weighting function lets us account for inflation of connections
          #due to multiplication of weighted code values
          #we do the same thing in weighted ENA
          this.code.matrix = this.code.matrix + f.weighting.function(this.connection.matrix)
          
          #go back to next row for this unit
          
        } #done looping over rows for this unit
        
    } #end of if statement -- the above is all skipped if there are no
      #non-zero rows to loop over for the unit
    
    #for this unit...
   
    #apply mask to the adjacency matrix
    if(use.mask) this.code.matrix = this.code.matrix * mask
  
    #first, deal with directed matrix
    
    #add directed adjacency matrix to the context
    f.contexts[[this.hoo]]$directed.adjacency = this.code.matrix
    
    #construct directed adjacency vector
    directed.adjacency.vectors = rbind(directed.adjacency.vectors, as.vector(this.code.matrix))
  
    
    #next, undirected
    
    #commenting this out keeps self-connections in undirecteed matrix
    #but it will be removed for ena adjacency vectors
    #diag(this.code.matrix) = 0
    
    #turn directed into undirected
    
    #upper triangle is some of directed connections
    upper_tri_indices <- upper.tri(this.code.matrix)
    this.code.matrix[upper_tri_indices] <- this.code.matrix[upper_tri_indices] + t(this.code.matrix)[upper_tri_indices]
    
    #make matrix symmetrical
    this.code.matrix[lower.tri(this.code.matrix)] = NA
    
    #add undirected matrix to context 
    f.contexts[[this.hoo]]$undirected.adjacancy = this.code.matrix
    
    #create undirected vector
    undirected.adjacency.vectors = rbind(
      undirected.adjacency.vectors,
      this.code.matrix[upper.tri(this.code.matrix)]
    )
   
    #go back to next context
  
  }#done looping over contexts
  
  #construct output
  output = list()
  
  #add contexts (with additions of adjacency matrices) to output
  output$contexts = f.contexts
  #add additional information for modeling
  output$directed.adjacency.vectors = directed.adjacency.vectors
  output$units = f.units
  
  #construct information for ENA set
  # browser()
  enadata = structure(list(), class = c("ENAdata", "list"))
  enadata$adjacency.matrix = rENA:::namesToAdjacencyKey(f.codes)
  enadata$codes = f.codes
  enadata$model = "TMA"
  enadata$raw = data.table::as.data.table(f.raw)
  enadata$units = f.units
  enadata$meta.data = data.table::as.data.table(output.meta.data)
  enadata$metadata = data.table::as.data.table(output.meta.data)
  enadata$adjacency.vectors = data.table::as.data.table(undirected.adjacency.vectors)
  enadata$accumulated.adjacency.vectors = data.table::as.data.table(undirected.adjacency.vectors)
  
  output$for.ena$enadata=enadata
  
  #if ena set is requested, construct ena set using rENA:::ena.set
  #and remove the information used to create it
  if(return.ena.set) {
    output$ena.set = rENA:::ena.set(enadata) #output$for.ena)
    output$for.ena = NULL
  }
  
  return(output)

}