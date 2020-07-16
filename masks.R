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
    print(mask)
    for (i in 1:nrow(df)){
      df[i,] = df[i,] * mask
    }
    output = df
  } else {
    output = df * mask
  }

  return(output)
}