step_function = function(window_size) {
  return(function(x) {
    if (x == 0) {
      return(0.5);
    } else if( x < window_size & x > 0) {
      return(1);
    } else {
      return(0);
    }
  })
}

continuous.decay.function.convex = function(x){
  if(x>=0 && x<1){
    return(-(1/18)*x^2 + (1/3)*x + (1/2))
  }
  else if(x>=1 && x<10){
    return((1/49)*(x-10)^2)
  }
  else if (x>=10){
    return(0)
  }
}

#define generic function to retuen decay calues
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


get.decay.modes = function(
  now,
  event.time=0,
  modes = NA,
  decay.function
) {

  #if modes not specifieed, assume a single decay function and call get.decay
  if (any(is.na(modes))) {
    return(get.decay(now, event.time, decay.function))
  }

  #again, trap POSIX if that is how times are given
  times = as.numeric(event.time)

  #compute lag
  x = as.numeric(now) - times

  #initialize output
  decays = rep(0, length(x))

  #abort if negative values are given
  if(any(x<0)) stop("negative values not permitted in decay times [get.decay.modes]")

  #get decays based on function given in list with name from modes
  #heart of the call here is the decay.function[[modes[i]]]
  #this pulls the function from the list decay.function[[  ]] with the same name modes[i]
  #as the mode of the current event i
  for (i in 1:length(x)) decays[i] = call_eq(x = x[i], eq = decay.function[[modes[i]]])

  #zero out negative results
  #necessary because polynomial decay functions can go negative outside of scope
  #(They can also get wildly positive, but I couldn't think how to trap that)
  decays[decays<0] = 0
  return(decays)
}