# If sender_cols, receiver_cols, or mode_column are empty, have our standin. Be sure to fill in 
# standin as expected in multidim_arr. 
# We do this so we don't fail when they don't specify senders/receivers/mode
preprocess <- function(
    multidim_arr, 
    sender_cols,
    receiver_cols,
    mode_colum
) {
  if (length(sender_cols) == 0) {
    sender_cols = list("standin")
  }
  if (length(receiver_cols) == 0) {
    receiver_cols = list("standin")
  }
  if (length(mode_column) == 0) {
    mode_column = list("standin")
  }
  
  # TODO: Process multidim array to handle this
    # multidimarray should be of shape (len(unique(sender)), len(unique(receiver)), 1, 2)
  
  # TODO. This should return a list with members being the new multidim_arr, sender_cols, receiver_cols, or mode_column
}

# Returns ground vectors to consider for a response line 
apply_windows <- function(
    multidim_arr, # multidimensional array [sender, receiver, modality, weight/window]
    sender_cols,
    receiver_cols,
    time_column,
    mode_column,
    i, # index in context
    context 
) {
  g_vecs <- list()
  r_i = context[[i,receiver_cols]]
  for (j in seq_along(0, i)) {
    potential_g_vec <- context[j,]
    s_j = potential_g_vec[[,sender_cols]]
    m_j = potential_g_vec[[,mode_column]]
    
    # Verify t_i - (t_j + window_{s_j, r_i, m_j}) \geq 0 
    val_to_check <- context[[i]][[time_column]] - (potential_g_vec[[j]] + multidim_arr[s_j, r_i, m_j, "window"])
    if (val_to_check >= 0) {
      g_vecs <- append(g_vecs, potential_g_vec)
    }
  }
  
  # This should return the actual ground vectors from the context i.e. not just the codes
  return(g_vecs)
}

# Applies weights to ground vectors for a response line 
apply_weights <- function(
    multidim_arr, # multidimensional array [sender, receiver, modality, weight/window]
    sender_cols,
    receiver_cols,
    mode_column,
    codes,
    i,
    context,
    g_vecs # ground vectors to weight 
) {
  # This should return the weighted ground vectors with just the codes
  g_w_vecs <- list()
  r_i = context[[i,receiver_cols]]
  for (vec in g_vecs) {
    s_j = potential_g_vec[[,sender_cols]]
    m_j = potential_g_vec[[,mode_column]]
    
    g_w_vec = multidim_arr[[s_j, r_i, m_j, "weight"]] * g_vec
    
    g_w_vecs <- append(g_w_vecs, g_w_vec)
  }
}

# 0 out diagonal of a square matrix
f_diag <- function(
    square_matrix
) {
  if (!is.matrix(mat) || nrow(mat) != ncol(mat)) {
    stop("Input must be a square matrix.")
  }
  
  mat[diag(nrow(mat))] <- 0
  return(mat)
}

#' accum_multidim
#'
#' @param multidim_arr Input multidimensional array. This should be in the format
#' [sender, receiver, modality, weight/window]
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
#' @return T/ONA (or T/ENA) accumulation
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
  # Preprocess multidim array
  result <- preprocess(multidim_arr, sender_cols, receiver_cols, mode_column)
  multidim_arr <- result$multidim_arr
  sender_cols <- result$sender_cols
  receiver_cols <- result$receiver_cols
  mode_column <- result$mode_column
  
  adj_vectors <- list()
  # For each unit
  for (unit in contexts[["model"]][["unit.labels"]]) { # assuming TMA generating contexts
    adj_vector <- list()
    # For each context, get unit's lines in that context (r_vec)
    # TODO: ensure context has column "standin" with values "standin" for all lines
    # TODO: ensure context only has codes we want to keep
    counting_matrices <- list()
    for (i in seq_along(contexts[["model"]][["contexts"]][[unit]])) { 
      r_vec = contexts[["model"]][["contexts"]][[unit]][[i]]
      if (r_vec$QEUNIT == unit) {
        # Get all lines before this line, which are individual g_vec, and 0 out those that
        # don't project far enough
        g_vecs <- apply_windows(multidim_arr, sender_cols, receiver_cols, time_column, mode_column,
                                i, context)

        # Query multidimensional array to reweight g^w_vec = w^{sr} * g_vec
        g_w_vecs <- apply_weights(multidim_arr, sender_cols, receiver_cols, mode_column, codes, i, context,
                                  g_vecs) 
          
        # Sum all g^w_vec together as g^{ws}_vec
        g_ws_vec <- Reduce("+", g_w_vecs)
          
        # Compute counting matrix = g^{ws}_vec r_vec^T + 0.5w^{rr} * f_diag(r_vec r_vec^T)
        response_sender_state <- r_vec[sender_cols]
        response_receiver_state <- r_vec[receiver_cols]
        response_mode_state <- r_vec[mode_column]
        w_rr = multidim_arr[response_sender_state, response_receiver_state, response_mode_state, "weight"]
        
        counting_matrix = matmul(g_ws_vec,t(r_vec)) + 0.5 * w_rr * f_diag(matmul(r_vec, t(r_vec)))
        counting_matrices = append(counting_matrices, counting_matrix)
      } else {
        # Do nothing 
      }
    }
  
    # Sum all counting matrices to get Omega matrix 
    omega_matrix = Reduce("+", counting_matrices)
    rownames(omega_matrix) = codes
    colnames(omega_matrix) = codes

    # Make into adjacency vector
      # Just do ONA version. need to keep upper triangle + binarize if using ENA
    row_col_combinations <- expand.grid(rownames(omega_matrix), colnames(omega_matrix))
    adj_vector <- paste(row_col_combinations[,1], "&", row_col_combinations[,2])
  
    # Apply normalization 
    l2_adj_vector <- sqrt(sum(adj_vector^2))
    normed_adj_vector <- adj_vector / l2_adj_vector
    
    # Save unit's adjacency vec
    adj_vectors[[unit]] <- normed_adj_vector
  }
  
  # Return accumulation (adjacency vecs for all units)
  return(adj_vectors)
}