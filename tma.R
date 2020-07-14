require(rENA)
library(rENA)
require(dplyr)
library(dplyr)

#for
require(openxlsx)
library(openxlsx)

# install.packages("optimbase")
require(optimbase)
library(optimbase)

setwd('/Users/arielfogel/Epistemic Analytics/tma');
source('./helpers.R');

# helper functions

d = read.csv('./test_data/TADMUSCoded.csv');
d = d %>%
  group_by(Team, Scenario, TimeImputed) %>%
  mutate(average_secs =
           case_when(
             str_detect(TimeImputed, '-') ~ 60/n(),
             TRUE ~ 30/n()
           )
  ) %>%
  group_by(Team, Scenario) %>%
  mutate(time_secs = cumsum(average_secs)) %>% ungroup;
mean_time = d[,c('average_secs')] %>% t %>% as.vector %>% mean;
d = d %>% group_by(Team) %>% arrange(X, .by_group = TRUE) %>% mutate(time = 1:n());

decay.function = function(x) {
  if (x==0) {
    return(.5)
  } else if(x<=5) {
    return(1)
  } else {
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

get.decay.modes = function(now, event.time=0, modes = NA, decay.function) {
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

#function to get contexts for each unit, by hoo.rules
#returns a list of dataframes (one for each context) in $contexts
#plus metadata that ena will need
get.contexts = function(units, data, hoo.rules){
  #This is a hack I use to create functions. When I start, I assign these
  #temporary names to variables. I can assign values and step through the function
  #while I am writing/deebugging. Then when the function works, I can create the function
  #call and pass the parameters to these termporary variables.
  f.data = data
  # TODO: BUG!!!!! this method can't handle units of a single column; must be 2 columns
  f.units = data.frame(units)
  f.hoo = hoo.rules

  #add row numbeers to the data (needed to keep data sorted)
  Rows = 1:nrow(f.data)
  f.data.augmented = cbind(Rows, f.data)

  #initialize output list for contexts
  hoos = list()
  #loop over units
  for(i in 1:nrow(f.units)) {
    this.unit = f.units[i,]

    #initialize this hoo
    hoos[[i]] = list()
    hoos[[i]][["context"]] = f.data.augmented[0,] #quick hack to create a blank copy of an existing matrix
    #get this unit data -- semi join takes thing from the left variable that match
    #things from the right variable in the columns in BY
    this.unit.data = semi_join(f.data.augmented, this.unit, by = names(this.unit))

    #assign meta data for unit
    #create a list of named list with names equal to unit columns
    for(j in names(this.unit)) hoos[[i]][["unit"]][j] = this.unit[,j]
    #put in values for this unit
    hoos[[i]][["unit"]] = as.data.frame(
      t(hoos[[i]][["unit"]]),
      col.names = names(hoos[[i]][["unit"]])
    )

    # for this unit, process each context rule
    for(this.rule in f.hoo){
      #initialize context with all data
      #we will remove things that don't fit
      this.output = f.data.augmented

      #loop over conditions in each rule to extract data
      for(k in 1:length(this.rule)){
        this.condition = this.rule[[k]]
        #get the column name this condition applies to
        this.col = names(this.rule[k])
        #replace <UNIT> with data from this unit and filter by those values
        if(this.condition == "<UNIT>") {
          #get values for column for this unit
          this.values = as.data.frame(unique(this.unit.data[,this.col]))
          names(this.values) = this.col
          #select rows with these values in the column
          this.output = semi_join(this.output, this.values, by = this.col)
        }
        else{ #or, select rows that match the given regex
          this.output = subset(
            this.output,
            grepl(
              this.condition,
              this.output[,this.col]
            )
          )
        }
      }#finished with all conditions for this rule

      #if this is the first rulw (or otherwise the list is empty), store temporary output
      if (nrow(hoos[[i]][["context"]]) == 0) hoos[[i]][["context"]] = this.output

      #otherwise, merge new temporary output into existing
      #This merge function removes duplicate rows and keeps output in the order
      #of column "Rows"
      else hoos[[i]][["context"]] = merge.on(hoos[[i]][["context"]], this.output, "Rows")


    }#finished with rules

    #remove row numbers
    hoos[[i]][["context"]] = hoos[[i]][["context"]][,-1]

  }#finished with unit

  #add unit names to context lists
  output.names = c()
  for (this.hoo in 1:length(hoos)) {
    output.names = c(output.names, paste(hoos[[this.hoo]]$unit, collapse = "::"))
  }
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


# d$Channel_Unimodal = 'ALL'

#specify hoo rules as list of lists
#note that options here are...
#   ...<UNIT>, which is replaced by the value of the individual unit
#               for which context is being constructed
#   ...any regex
speaker_units = c('COND', 'Team', 'MacroRole', 'Speaker');
meta = c('MacroRole');
conversation = c('Team', 'Scenario');
my.hoo.usualENA = list()
my.hoo.usualENA[[1]] = list(Team = "<UNIT>", Scenario = "<UNIT>")


# my.hoo.TMA = list()
# my.hoo.TMA[[1]] = list(Condition = "<UNIT>", GroupName = "<UNIT>", Context = "Public")
# my.hoo.TMA[[2]] = list(Condition = "<UNIT>", UserName = "<UNIT>")

#set units
#get all possible units in the set
my.units = unique(d[,c('COND', 'Team', 'MacroRole', 'Speaker')])
#use only a few for this analysis
# my.units = my.units[1:3,]

#generate contexts for this data: REGULAR ENA
hoos.regular = get.contexts(units = my.units, data = d, hoo.rules = my.hoo.usualENA)
my.hoo.usualENA = list()
my.hoo.usualENA[[1]] = list(Team = "<UNIT>", Scenario = "<UNIT>")
hoos.regular$contexts$`Experimental::3/31/99::Other::KUWAIT TOWER`$context
these.hoos = hoos.regular

accumulate.units = function(context.object,
                            time.column,
                            codes,
                            decay.function = default.decay.function,
                            mode.column = NA,
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

  #initialize metadata matrix, if needed
  output.meta.data = ifelse(use.meta, f.raw[0,c(names(f.units),f.meta.data)], NA)

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
  #loop over contexts
  for (this.hoo in 1:length(f.contexts)) {
    #initialize connection  matrix for this unit
    this.code.matrix = blank.code.matrix
    #get data from this context
    this.context = f.contexts[[this.hoo]]$context %>% as.data.frame
    #remove rows with no codes from the data
    #we don't need them as "spacers" for the rest of the data
    #because we are using time to define our connections
    this.context = this.context[rowSums(as.matrix(this.context[,f.codes])) > 0,]
    #set row numbers
    Rows = 1:nrow(this.context)
    f.data.augmented = cbind(Rows, this.context)
    #get unit name
    this.unit = f.contexts[[this.hoo]]$unit
    #find rows belonging to unit
    this.unit.data = semi_join(f.data.augmented, this.unit, by = names(this.unit))
    #get meta data for this unit and add to the the meta data dataframe
    if(use.meta) {
      output.meta.data = rbind(output.meta.data, get.unique.col.values(this.unit.data[,f.meta.data]))
    }
    #if there are non-zero rows for this unit, compute adjacencies
    if(nrow(this.unit.data) > 0) {
      #get rows associated with this unit
      unit.rows = this.unit.data$Rows
      this.time = this.context[,f.time]
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
        #compute decays, Get modes, if needed
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
    this.code.matrix[upper.tri(this.code.matrix)] =
      this.code.matrix[upper.tri(this.code.matrix)] + this.code.matrix[lower.tri(this.code.matrix)]
    #make matrix symmetrical
    this.code.matrix[lower.tri(this.code.matrix)] = this.code.matrix[upper.tri(this.code.matrix)]
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
  enadata = structure(list(), class = c("ENAdata", "list"))
  enadata$adjacency.matrix = rENA:::namesToAdjacencyKey(f.codes)
  enadata$codes = f.codes
  enadata$model = "TMA"
  enadata$raw = data.table::as.data.table(f.raw)
  enadata$units = f.units
  enadata$meta.data = data.table::as.data.table(output.meta.data)
  enadata$metadata = f.units
  enadata$adjacency.vectors = undirected.adjacency.vectors %>% data.table::as.data.table()
  enadata$accumulated.adjacency.vectors = undirected.adjacency.vectors %>% data.table::as.data.table()

  output$for.ena$enadata=enadata

  #if ena set is requested, construct ena set using rENA:::ena.set
  #and remove the information used to create it
  if(return.ena.set) {
    browser();
    output$ena.set = rENA:::ena.set(enadata) #output$for.ena)
    output$for.ena = NULL
  }
  return(output)
}

chat.decay.function = decay.function

#decay function with a longer decay time
# view.decay.function = function(x) return(-0.0039*x^3 + 0.0661*x^2 - 0.3507*x + 1)
view.decay.function = decay.function



#construct list for passing to accumulate,units
modal.decay.function = list()
modal.decay.function$Private = view.decay.function
modal.decay.function$Public = chat.decay.function

my.accumulation = accumulate.units(context.object = these.hoos,
                                   codes = TADMUS_code_cols,
                                   time.column = "time",
                                   #decay.function = decay.function,
                                   decay.function = modal.decay.function,
                                   mode.column = NA,
                                   weight.by = function(x) {x*1},
                                   time.limit = 20,
                                   # meta.data = c('MacroRole'),
                                   # meta.data = c("CONFIDENCE.Pre", "CONFIDENCE.Change"),
                                   return.ena.set = TRUE)

#show results
print("For ENA:")
my.accumulation$ena.set
my.accumulation$directed.adjacency.vectors
my.accumulation$contexts
my.accumulation$units
# my.accumulation$for.ena

print("Contexts (with adjacency matrices):")
my.accumulation$contexts
print("units:")
my.accumulation$units
print("Directed adjacency vectors")
my.accumulation$directed.adjacency.vectors
print("::")

#show delays for the two functions
# print("original decay function from t=0 to t=9")
# chat.decay.function( (1:10)-1 )
# print("Slower decay function from t=0 to t=9")
# view.decay.function( (1:10)-1 )
set <- ena.make.set(my.accumulation$ena.set)

experimental_command_points =  set$points$COND$Experimental[set$points$COND$Experimental[, MacroRole == 'Command']];
experimental_command_lineweights = as.matrix(set$line.weights$COND$Experimental[set$line.weights$COND$Experimental[, MacroRole == 'Command']])
experimental_command_mean = as.vector(colMeans(experimental_command_lineweights))
control_command_points =  set$points$COND$Control[set$points$COND$Control[, MacroRole == 'Command']];
control_command_lineweights = as.matrix(set$line.weights$COND$Control[set$line.weights$COND$Control[, MacroRole == 'Command']])
control_command_mean = as.vector(colMeans(control_command_lineweights))
subtracted_mean = control_command_mean - experimental_command_mean

plot <- ena.plot(enaset = ena_set, title = "TMA Plot", dimension.labels = c("SVD1", "SVD2"))
plot <- ena.plot.points(plot, points = ena_set$points, colors = c("red", "blue", "green"))
# plot <- ena.plot.network(plot, network = list(ena_set$line.weights[2,]), colors = c("blue"))

plot$plot