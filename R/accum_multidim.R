#' accum_multidim
#'
#' @param multidim_arr Input multidimensional array. This should be in the format
#' [sender, receiver, modality, weight, window]
#' @param sender_cols Sender columns
#' @param receiver_cols Receiver columns
#' @param time_column Time column in contexts
#' @param mode_column Modality column in contexts
#' @param codes Codes of interest
#' @param contexts Contexts for each unit of analysis
#' @param decay Which decay function to use. By default, this is a simple window 
#' @param calculate_adj_vectors_using How to compute adj vectors. By default, this is matmul(g^T, r).
#' @param adj_vec_transform How to transform adj. vecs e.g. binarization. By default, this is empty  
#' @param accumulate_unit_vectors_by How to compute final adjacency vector from adjacency vectors
#' accross all of a unit's utterances; by default, this is colSums 
#' @param norm_by How to normalize. By default, this is w.r.t. the \ell_2 norm 
#' @param summarize_ground_using How to combine ground into one vec. By default, this is colSums
#' @param return_ena_set Whether or not to return an ENA set. By default, this is ONA
#' 
#' @return ONA or ENA accumulation
#' @export
accum_multidim <- function(
    multidim_arr, 
    sender_cols,
    receiver_cols,
    time_column,
    mode_column,
    codes,
    contexts,
    decay = decay(simple_window),
    calculate_adj_vectors_using = inner_product_ground_response,
    adj_vec_transform = NA,
    accumulate_unit_vectors_by = colSums,
    norm_by = "l2",
    summarize_ground_using = colSums,
    return_ena_set = FALSE,
    ...
) {
  # For each unit, context
  
    # For each unit's lines in that context (r_vec)
  
      # Look back at all lines before this line (g_vec) and query multidimensional array (w^g_{sr})
  
      # Let g^w_vec be w^g_{sr} * g_vec 
  
      # Sum all g^w_vec together for those g^w_vec that project far enough forward w.r.t. time 
      # as g^{ws}_vec
        # Depends on summarize_ground_using
        # Depends on decay function; if decay function is not a simple window, we need to reweight here
  
      # Compute matmul(g^{ws}_vec^T,r) + 0.5w_{rr} * matmul(r_vec^T, r_vec)
  
      # Sum all g_vec^T r_vec to get Omega matrix 
        # Depends on accumulate_unit_vectors_by 
  
      # Make into adjacency vector
        # If using ENA, keep only upper triangle (not including diag) as adjacency vector 
    
      # Apply normalization 
        # Depends on norm_by 
  
      # Save unit's adjacency vec
  
    # Return accumulation (adjacency vecs for all units)
  
  return(0)
}

# Notes:
# 1) Where does binaralization get applied; before or after weighting the ground vectors? 