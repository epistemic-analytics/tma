#' Mask function
#'
#' @param df data.frame to apply mask on
#' @param mask mask vector with ncol same as df, or matrix with same dims as original
#' @param convertMaskToVector if TRUE (default), mask is in the form of an adjacency matrix, which gets converted to a vector
#' @param undirectedModel setting for directed or undirected model (only used if convertMaskToVector = TRUE)
#' @param dfIsMatrixOfVectors when TRUE (default), df is a set of adjacency vectors; when false, df is ____this was missing___
#'
#' @return TBD
#' @export
mask = function(df, mask, convertMaskToVector = TRUE, undirectedModel = TRUE, dfIsMatrixOfVectors = TRUE ) {
  if (dfIsMatrixOfVectors) {
    if (convertMaskToVector) {
      if(undirectedModel) {
        mask = mask[upper.tri(mask)]
      }
      else {
        mask = as.vector(mask)
      }
    } 
    
    # print(mask)
    for (i in 1:nrow(df)) {
      df[i,] = df[i,] * mask
    }
    
    output = df
    
  }
  else {  
    output = df * mask
  }
  
  return(output)
}
