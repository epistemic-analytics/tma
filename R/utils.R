#' Replace factors
#'
#' @param x factor 
#'
#' @return convert factor to character vector, numeric if possible
#' @export
factor.to.value = function(x) {
  y = as.character(x)
  z = suppressWarnings(as.numeric(y))
  if(any(is.na(z))) return(y)
  return (z)
}

#' function to convert timestamps
#'
#' @param time.column TBD
#' @param format TBD
#' @param precision TBD
#' @param zero TBD
#' @param units TBD
#'
#' @return TBD
#' @export
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

#' function allows a call to a functionA inside functionB where functionA can be a...
#' 
#' @details  eq can be 1 of 3 things:
#' 1. function definition
#  2. an equation written as character string
#  3. a function name as character string 
#' 
#' @param x number to use as parameter to eq
#' @param eq See Details.
#'
#' @return result of calling eq
#' @export
call_eq <- function(x = 1, eq = "x^3 + 2*x^2 + x - 7") {
  eq_sub <- substitute(eq)
  
  if(is.function(eq)) {
    return(eq(x));
  }
  
  else if(is.call(eq_sub)) {
    res <- eval(substitute(eq))
    if (is.character(res)) {
      return(call_eq(x = x, eq = res));
    }
    else {
      return(res);
    }
  }
  
  else if(inherits(eq, "lm")) {
    as_eq <- lm_to_eq(eq);
    res <- eval(substitute(as_eq))
    return(call_eq(x = x, eq = res));  
  }
  
  else {
    # I'm not clear what this was doing, as it didn't return and the next line isn't a part of an 
    # else {}, so the next line will always execute and be what is returned, likely producing
    # unexpected results. (clm)
    # if(is.name(eq_sub)) {
    #   eval(substitute(eval(eq_sub)))
    # }
    
    env <- new.env();
    res <- eval(parse(text = eq), envir = env);
    
    env_objs <- ls(envir = env);
    if(length(env_objs) == 1) {
      return(get(ls(envir = env)[1], envir = env));
    }
    else {
      return(res);
    }
  }
}

#' Merge using data.tables
#' 
#' @description function to merge and reordeer based on column of row numbers that needs to be specified
#' @param x TBD
#' @param y TBD
#' @param order.column TBD
#'
#' @return merged data.frame
#' @export
merge_on = function(x, y, order.column = NA) {
  if(any(names(x) != names(y))) {
    stop("Objects x and y must have the same column names")
  }
  
  # if row number column is not specified, try to use the first column
  # NOTE: This seems dangerous to me, at the very least we could search for an ID column (column)
  #       with a unique value in each row (clm)
  if (is.na(order.column)) {
    order.column = names(x)[1]
  }
  if (length(order.column) > 1) {
    stop("Can only sort on one column (order.column must be a single column name or number)")
  }
  
  output <- rbind(x, y);;
  output <- output[!duplicated(output),];
  output <- output[order(output[[order.column]]), ];
  
  return(output);
}


#' get.unique.col.values
#' 
#' @description function takes a data frame and returns the value for each column if it has one
#' or, if there is more than one value in a column, returns NA for that column
#'
#' @param df TBD
#'
#' @return TBD
#' @export
get.unique.col.values = function(df){
  output = data.frame(matrix(NA, nrow = 1, ncol = ncol(df)))
  names(output) = names(df)
  for (this.col in names(df)) {
      this.value = unique(df[,this.col])
      if(length(this.value) == 1) output[1,this.col] = this.value
  } 
  return(output)
}

#' make test data frames
#'
#' @description creates df by rows rather than columns (which I find more intutive)
#' @param data TBD
#' @param rows TBD
#' @param columns TBD
#'
#' @return data.frame
#' @export
test.df = function(data, rows = NA, columns = NA){
  if(is.na(rows)) {
    if(is.na(columns)) {
      stop("Need rows or columns")
    }
    else {
      rows = length(data)/columns
    }
  }
  
  if(is.na(columns)) {
    columns = length(data)/rows
  }
  return(
    data.frame(
      t(
        matrix(data, nrow = columns, ncol = rows)
      )
    )
  )
}


#' Quote Columns
#'
#' @description used to get column names from a data frame in a way that makes them easy to 
#' put into a function call 
#' @param x TBD
#' @param as.one.line TBD
#'
#' @return TBD
#' @export
get.quoted.columns = function(x, as.one.line = FALSE){
  n = names(x)
#  for (i in n) cat('"', i,'"', ifelse(as.one.line, "' ", ",\n"))
  for (i in n) cat("'", i, "'", ifelse(as.one.line, ", ", ",\n"), sep = "")
  return(NULL)
}
