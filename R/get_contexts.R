#' get contexts
#'
#' @description get contexts for each unit, by hoo.rules plus metadata that ena will need
#' @param units TBD
#' @param data TBD
#' @param hoo.rules TBD
#'
#' @return list of dataframes (one for each context) in $contexts
#' @export
get.contexts = function(units, data, hoo.rules) {
    #This is a hack I use to create functions. When I start, I assign these
    #temporary names to variables. I can assign values and step through the function
    #while I am writing/deebugging. Then when the function works, I can create the function
    #call and pass the parameters to these termporary variables.
    f.data = data
    f.units = units
    f.hoo = hoo.rules

    #add row numbeers to the data (needed to keep data sorted)
    # Rows = 1:nrow(f.data) %>% tibble %>% set_names('Rows')
    Rows <- data.frame(Rows = seq(nrow(f.data)));
    f.data.augmented = cbind(Rows, f.data)
    
    #initialize output list for contexts
    hoos = list()
    
    #loop over units
    for(i in 1:nrow(f.units)){
      this.unit = f.units[i,]
    
      #initialize this hoo
      hoos[[i]] = list()
      hoos[[i]][["context"]] = f.data.augmented[0,] #quick hack to create a blank copy of an
                                                    #existing matrix
      
      #get this unit data -- semi join takes thing from the left variable that match
      #things from the right variable in the columns in BY
      # this.unit.data = dplyr::semi_join(f.data.augmented, this.unit, by = names(this.unit))
      
      ## I'm not sure if the column ordering the reason behind using dplyr here, so I've added an additional [,] after
      ## merge so the results are identical to the call to `semi_join`, if not, then we should just use the option here.
      # this.unit.data <- merge(f.data.augmented, this.unit, by = names(this.unit))
      # this.unit.data <- merge(f.data.augmented, this.unit, by = names(this.unit))[, names(f.data.augmented)]
      this.unit.data <- as.data.frame(data.table(f.data.augmented)[data.table(this.unit), names(f.data.augmented), on = names(this.unit), with = FALSE])
      
      #assign meta data for unit
      #create a list of named list with names equal to unit columns
      for(j in names(this.unit)) hoos[[i]][["unit"]][j] = this.unit[,j]
      #put in values for this unit
      hoos[[i]][["unit"]] =  as.data.frame(
        t(hoos[[i]][["unit"]]), 
        col.names = names(hoos[[i]][["unit"]]),
        stringsAsFactors = FALSE
      )
 
      # for this unit, process each context rule 
      for(this.rule in f.hoo) {
        #initialize context with all data
        #we will remove things that don't fit
        this.output = f.data.augmented
        
        #loop over conditions in each rule to extract data
        for(k in 1:length(this.rule)) {
          
          this.condition = this.rule[[k]]
          #get the column name this condition applies to
          this.col = names(this.rule[k])
          
          #replace <UNIT> with data from this unit and filter by those values
          if(this.condition == "<UNIT>") {
            #get values for column for this unit
            this.values = as.data.frame(unique(this.unit.data[,this.col]), stringsAsFactors = FALSE)
            names(this.values) = this.col
            #select rows with these values in the column
            # this.output = merge(this.output, this.values, by = this.col)
            
            ## See comment above re: semi_join and column ordering
            # this.output <- merge(this.output, this.values, by = this.col)[, colnames(this.output)]
            # this.output <- as.data.frame(data.table(this.output)[this.values, colnames(this.output), on = this.col, with = FALSE])
            this.output <- as.data.frame(data.table(this.output)[data.table(this.values), colnames(this.output), on = this.col, with = FALSE])
          }
          
          #or, select rows that match the given regex
          else{
            this.output = subset(
              this.output, grepl(this.condition, this.output[,this.col])
            )
          }
        }#finished with all conditions for this rule
      
        #if this is the first rulw (or otherwise the list is empty), store temporary output
        if (nrow(hoos[[i]][["context"]]) == 0) hoos[[i]][["context"]] = this.output
    
        #otherwise, merge new temporary output into existing
        #This merge function removes duplicate rows and keeps output in the order
        #of column "Rows"
        else {
          # browser()
          hoos[[i]][["context"]] = merge_on(hoos[[i]][["context"]], this.output, "Rows")
        }
    
              
      }
      # finished with rules
    
      #remove row numbers
      hoos[[i]][["context"]] = hoos[[i]][["context"]][, -1]
    }
    #finished with unit  
  
  #add unit names to context lists
  # output.names = c()
  # for (this.hoo in 1:length(hoos)) {
  #   output.names = c(output.names, paste(hoos[[this.hoo]]$unit, collapse = "::"))
  # }
  output.names <- sapply(lapply(hoos, `[[`, "unit"), paste, collapse = "::");
  names(hoos) = output.names
  
  #begin constructing output

  output = list()             #initialize output list
  output$contexts = hoos      #store named contexts
  output$hoo.rules = f.hoo    #add rules used
  
  #add information ENA needs
  output$units = f.units      #units  
  output$raw.data = f.data    #raw data

  return(output)

}

#' get contexts
#'
#' @description get contexts for each unit, by hoo.rules plus metadata that ena will need
#' @param units TBD
#' @param data TBD
#' @param hoo.rules TBD
#'
#' @return list of dataframes (one for each context) in $contexts
#' @export
get.contexts_dt = function(units, data, hoo.rules) {
    #This is a hack I use to create functions. When I start, I assign these
    #temporary names to variables. I can assign values and step through the function
    #while I am writing/deebugging. Then when the function works, I can create the function
    #call and pass the parameters to these termporary variables.
    f.data = data
    f.units = units
    f.hoo = hoo.rules

    #add row numbeers to the data (needed to keep data sorted)
    Rows = 1:nrow(f.data)
    f.data.augmented = cbind(Rows, f.data)
    
    #initialize output list for contexts
    hoos = list()
    
    #loop over units
    for(i in 1:nrow(f.units)){
      this.unit = f.units[i,]
    
      #initialize this hoo
      hoos[[i]] = list()
      hoos[[i]][["context"]] = f.data.augmented[0,] #quick hack to create a blank copy of an
                                                    #existing matrix
      
      #get this unit data -- semi join takes thing from the left variable that match
      #things from the right variable in the columns in BY
      # this.unit.data = dplyr::semi_join(f.data.augmented, this.unit, by = names(this.unit))
      
      ## I'm not sure if the column ordering the reason behind using dplyr here, so I've added an additional [,] after
      ## merge so the results are identical to the call to `semi_join`, if not, then we should just use the option here.
      # this.unit.data <- merge(f.data.augmented, this.unit, by = names(this.unit))
      this.unit.data <- merge(f.data.augmented, this.unit, by = names(this.unit))[, names(f.data.augmented)]
      
      #assign meta data for unit
      #create a list of named list with names equal to unit columns
      for(j in names(this.unit)) hoos[[i]][["unit"]][j] = this.unit[,j]
      #put in values for this unit
      hoos[[i]][["unit"]] =  as.data.frame(
        t(hoos[[i]][["unit"]]), 
        col.names = names(hoos[[i]][["unit"]]),
        stringsAsFactors = FALSE
      )
 
      # for this unit, process each context rule 
      for(this.rule in f.hoo) {
        #initialize context with all data
        #we will remove things that don't fit
        this.output = f.data.augmented
        
        #loop over conditions in each rule to extract data
        for(k in 1:length(this.rule)) {
          
          this.condition = this.rule[[k]]
          #get the column name this condition applies to
          this.col = names(this.rule[k])
          
          #replace <UNIT> with data from this unit and filter by those values
          if(this.condition == "<UNIT>") {
            #get values for column for this unit
            this.values = as.data.frame(unique(this.unit.data[,this.col]), stringsAsFactors = FALSE)
            names(this.values) = this.col
            #select rows with these values in the column
            # this.output = merge(this.output, this.values, by = this.col)
            
            ## See comment above re: semi_join and column ordering
            this.output <- merge(this.output, this.values, by = this.col)[, colnames(this.output)]
          }
          
          #or, select rows that match the given regex
          else{
            this.output = subset(
              this.output, grepl(this.condition, this.output[,this.col])
            )
          }
        }#finished with all conditions for this rule
      
        #if this is the first rulw (or otherwise the list is empty), store temporary output
        if (nrow(hoos[[i]][["context"]]) == 0) hoos[[i]][["context"]] = this.output
    
        #otherwise, merge new temporary output into existing
        #This merge function removes duplicate rows and keeps output in the order
        #of column "Rows"
        else {
          # browser()
          hoos[[i]][["context"]] = merge_on(hoos[[i]][["context"]], this.output, "Rows")
        }
    
              
      }
      # finished with rules
    
      #remove row numbers
      hoos[[i]][["context"]] = hoos[[i]][["context"]][, -1]
    }
    #finished with unit  
  
  #add unit names to context lists
  # output.names = c()
  # for (this.hoo in 1:length(hoos)) {
  #   output.names = c(output.names, paste(hoos[[this.hoo]]$unit, collapse = "::"))
  # }
  output.names <- sapply(lapply(hoos, `[[`, "unit"), paste, collapse = "::");
  names(hoos) = output.names
  
  #begin constructing output

  output = list()             #initialize output list
  output$contexts = hoos      #store named contexts
  output$hoo.rules = f.hoo    #add rules used
  
  #add information ENA needs
  output$units = f.units      #units  
  output$raw.data = f.data    #raw data

  return(output)
}