#function to convert timestamps
convert.time = function(time.column,
                        format = "%m/%d/%Y %H:%M",
                        precision = 4,
                        zero = TRUE,
                        units = "minutes"
){

  #fix number of significant digits
  options(digits.secs = precision)
  #convert to POSIX time (time in seconds since 1970-01-01 00:00.00 UTC)
  #see function documentation for details
  posix.time = as.POSIXct(time.column, format=format )

  #in most cases we just want a number, as POSIX does funny things to dates
  times = as.numeric(posix.time)
  #if requested, set earliest date in set to 0
  if (zero) times = times - min(times)

  #convert to time scale
  #if units are none of these, the return DAYS
  if(units != "seconds") {times = times / 60
  } else {if(units != "minutes") {times = times / 60
  } else {if(units != "hours") {times = times /24}
  }}

  return( times )
}


#function allows a call to a functionA inside functionB where functionA can be a...
#   ...fucntion call
#   ...an equation written out
#   ...a character string for a function

call_eq <- function(x = 1, eq = "x^3 + 2*x^2 + x - 7") {
  eq_sub <- substitute(eq)
  if(is.function(eq)) {
    eq(x)
  }
  else if(is.call(eq_sub)) {
    eval(substitute(eq))
  }
  else {
    if(is.name(eq_sub)) {
      eval(substitute(eval(eq_sub)))
    }
    eval(parse(text = eq))
  }
}

#function to merge and reordeer based on column of row numbers that needs to be specified
#uses a bit of tidyverse trickery
#adds
merge.on = function(x, y, order.column = NA) {
  if(any(names(x) != names(y))) stop("Objects x and y must have the same column names")
  #get rows that are only in x
  unique.x = anti_join(x,y, by = names(x))
  #add rows only inb x to all of y (since y+x will now have all of x and all of y without repeats)
  output = rbind(unique.x,y)
  #if row number column is not specified, try to use the first column
  if (is.na(order.column)) order.column = names(x)[1]
  if (length(order.column) > 1) stop(
    "Can only sort on one column (order.column must be a single column name or number)")
  #order and return
  output = output[order(output[,order.column]), ]
  return(output)
}


# get.decay was originally for RSData
#define generic function to return decay values
#TIME IS IN MINUTES
get.decay = function(now, event.time=0, decay.function) {
  #trap for POSIX times
  times = as.numeric(event.time)
  #compute lag
  x = as.numeric(now) - times
  #trap negative lag times (ie, events in the future)
  ok.values = (x>=0)
  #initialize output
  output = rep(0, length(x))
  if(any(ok.values)) {
    #compute decay
    decays = call_eq(x = x[ok.values], eq = decay.function(x))
    #zero out negative decays
    decays[decays<0] = 0
    output[ok.values] = decays
  }
  return(output)
}

#function takes a data frame and returns the value for each column if it has one
##or, if there is more than one value in a column, returns NA for that column
get.unique.col.values = function(df){
  output = data.frame(matrix(NA, nrow = 1, ncol = ncol(df)))
  names(output) = names(df)
  for (this.col in names(df)) {
    this.value = unique(df[,this.col])
    if(length(this.value) == 1) output[1,this.col] = this.value
  }
  return(output)
}

#used to make test data frames
#creates df by rows rather than columns (which I find more intutive)
test.df = function(data, rows = NA, columns = NA){
  if(is.na(rows)) if(is.na(columns)) {stop("Need rows or columns")
  } else {rows = length(data)/columns}
  if(is.na(columns)) columns = length(data)/rows
  return(data.frame(t(matrix(data, nrow = columns, ncol = rows))))
}

#used to get column names from a data frame in a way that makes them easy to
#put into a function call
get.quoted.columns = function(x, as.one.line = FALSE){
  n = names(x)
  #  for (i in n) cat('"', i,'"', ifelse(as.one.line, "' ", ",\n"))
  for (i in n) cat("'", i, "'", ifelse(as.one.line, ", ", ",\n"), sep = "")
  return(NULL)
}

mask = function(df,                         #data frame to apply mask on
                mask,                       #mask (vector with ncol same as df, or matrix with
                #same size as original)
                convertMaskToVector = T,    #if true, mask is in the for of an adjacency matrix,
                #which gets converted to a vector
                undirectedModel = T,        #setting for directed or undirected model (only used
                #if convertMaskToVector = TRUE)
                dfIsMatrixOfVectors = T){   #when true, df is a set of adjacency vectors;
  #when false, df is
  if (dfIsMatrixOfVectors) {
    if (convertMaskToVector) {
      if(undirectedModel) {
        mask = mask[upper.tri(mask)]
      } else {
        mask = as.vector(mask)
      }
    }
    for (i in 1:nrow(df)){
      df[i,] = df[i,] * mask
    }
    output = df
  } else {
    output = df * mask
  }
  return(output)

}
