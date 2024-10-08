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
  adj_vectors <- list()
  # For each unit
  for (unit in contexts[["model"]][["unit.labels"]]) { # assuming TMA generating contexts
    adj_vector <- list()
    # For each context, get unit's lines in that context (r_vec)
    counting_matrices <- list()
    for (i in seq_along(contexts[["model"]][["contexts"]][[unit]])) { 
      r_vec = contexts[["model"]][["contexts"]][[unit]][[i]]
      if (r_vec$QEUNIT == unit) {
        # Look back at all lines before this line, which are individual g_vec  
        g_vecs <- list() # TODO 
        
        # 0 out all those g_vec that are not in window size
        g_vecs <- list() # TODO 
        
        # Query multidimensional array to reweight g^w_vec = w^{sr} * g_vec
        g_w_vecs <- list() # TODO 
        
        # Sum all g^w_vec together as g^{ws}_vec
        g_ws_vec <- list() # TODO. 
          
        # Compute counting matrix = g^{ws}_vec r_vec^T + 0.5w^{rr} * r_vec r_vec^T
        # TODO 
        counting_matrix = matmul(g_ws_vec,t(r_vec)) + 0.5 * multidim_arr[...] * matmul(r_vec, t(r_vec))
        counting_matrices = append(counting_matrices, counting_matrix)
      } else {
        # Do nothing 
      }
    }
  
    # Sum all counting matrices to get Omega matrix 
    omega_matrix = list() # TODO 

    # Make into adjacency vector
    # TODO; just do ONA version. need to keep upper triangle + binarize if using ENA
  
    # Apply normalization 
    # TODO 
  
    # Save unit's adjacency vec
    adj_vectors[[unit]] <- adj_vector
  }
  
  # Return accumulation (adjacency vecs for all units)
  return(adj_vectors)
}