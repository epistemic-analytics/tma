# If sender_cols, receiver_cols, or mode_column are empty, have our standin. Be sure to fill in 
# standin as expected in multidim_arr. 
# We do this so we don't fail when they don't specify senders/receivers/mode
# note: works for no sender, no receiver, modality
preprocess <- function(
    multidim_arr, 
    unique_senders,
    unique_receivers,
    unique_modes,
    sender_cols,
    receiver_cols,
    mode_column
) {
  old_unique_senders <- unique_senders
  old_unique_receivers <- unique_receivers
  old_unique_modes <- unique_modes
  if (length(old_unique_senders) == 0) {
    old_unique_senders <- NA  # Placeholder for missing senders
  }
  if (length(old_unique_receivers) == 0) {
    old_unique_receivers <- NA  # Placeholder for missing receivers
  }
  
  
  # Ensure standin is present for any empty columns
  if (length(sender_cols) == 0) {
    sender_cols <- "standin"
    unique_senders <- c("standin")
  }
  if (length(receiver_cols) == 0) {
    receiver_cols <- "standin"
    unique_receivers <- c("standin")
  }
  if (length(mode_column) == 0) {
    mode_column <- "standin"
    unique_modes <- c("standin")
  }
  
  # Create a new multidimensional array to hold expanded values
  weight_window_axis = c("weight", "window")
  new_dimnames <- list(
    senders = unique_senders,
    receivers = unique_receivers,
    modes = unique_modes,
    weight_window = weight_window_axis
  )
  new_dims = c(length(unique_senders), 
          length(unique_receivers), 
          length(unique_modes), 
          length(weight_window_axis))
  new_multidim_arr <- array(0, dim = new_dims, dimnames = new_dimnames)

  
  combinations <- expand.grid(new_senders = unique_senders,
                              new_receivers = unique_receivers,
                              new_modes = unique_modes,
                              old_senders = old_unique_senders,
                              old_receivers = old_unique_receivers,
                              old_modes = old_unique_modes,
                              stringsAsFactors = FALSE)

  
  combinations <- combinations[
    (# just modality
    (combinations$new_senders == "standin" & combinations$new_receivers == "standin" & combinations$new_modes == combinations$old_modes) | 
    # modality + sender
    (combinations$new_senders ==  combinations$old_senders & combinations$new_receivers == "standin" & combinations$new_modes == combinations$old_modes) | 
    # modality + receiver
    (combinations$new_senders == "standin" & combinations$new_receivers == combinations$new_receivers & combinations$new_modes == combinations$old_modes) | 
    # modality + sender + receiver
    (combinations$new_senders == combinations$old_senders & combinations$new_receivers == combinations$new_receivers & combinations$new_modes == combinations$old_modes)) 
    ,
  ]
  

  for (i in seq_len(nrow(combinations))) {
    new_s <- combinations$new_senders[i]
    new_r <- combinations$new_receivers[i]
    new_m <- combinations$new_modes[i]
    orig_s <- combinations$old_senders[i]
    orig_r <- combinations$old_receivers[i]
    orig_m <- combinations$old_modes[i]
  
    old_index_list <- Filter(Negate(is.na), list(orig_s, orig_r, orig_m))
    
    new_multidim_arr[new_s, new_r, new_m, "weight"] <- do.call(`[`, c(list(multidim_arr), old_index_list, list("weight")))
    new_multidim_arr[new_s, new_r, new_m, "window"] <- do.call(`[`, c(list(multidim_arr), old_index_list, list("window")))
  
  }
  
  
  # Return the updated multidimensional array and the new column identifiers
  result <- list(
    multidim_arr = new_multidim_arr,
    sender_cols = sender_cols,
    receiver_cols = receiver_cols,
    mode_column = mode_column
  )
  
  return(result)
}

# Returns ground vectors to consider for a response line 
apply_windows <- function(
    multidim_arr, # multidimensional array [sender, receiver, modality, weight/window]
    sender_cols,
    receiver_cols,
    time_column,
    mode_column,
    i, # index of response line in context
    context, 
    codes # for edge case w/ i == 1 
) {
  g_vecs <- list()
  g_vecs_counter = 1
  # ground is at j, response is at i
  r_i = context[[i,receiver_cols]]
  # print(context[i,]) 
  
  if (i == 1) { # first line is response line--since we aren't including response lines, just provide a dummy vec w/ 0s @ codes
    temp_vec <- context[i,]
    temp_vec[,codes] <- 0
    g_vecs[[g_vecs_counter]] <- temp_vec
    return(g_vecs)
  }
  
  for (j in seq(1, i-1)) {
    s_j = pull(context[j, ..sender_cols])
    m_j = pull(context[j, ..mode_column])
    
    # Verify t_i + w_{s_j, r_i, m_j} \geq t_j
    val_to_check <- pull(context[j, ..time_column]) + multidim_arr[s_j, r_i, m_j, "window"] # this is fine
    # print(val_to_check)
    if (val_to_check >= pull(context[i, ..time_column])) {
      # print(context[j,]) # this is... right? 
      g_vecs[[g_vecs_counter]] <- context[j,]
      g_vecs_counter = g_vecs_counter + 1 
    }
  }
  
  # print(g_vecs_counter) 
  
  # print(i)
  # print(g_vecs) 
  
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
    i, # index of response line in context
    context,
    g_vecs # ground vectors to weight 
) {
  # This should return the weighted ground vectors with just the codes
  g_w_vecs <- list()
  g_w_vecs_counter = 1 
  # ground is at j, response is at i
  r_i = pull(context[i, ..receiver_cols])
  for (vec in g_vecs) {
    s_j = pull(vec[,..sender_cols])
    m_j = pull(vec[,..mode_column])
    
    unweighted <- as.numeric(unname(c(vec[,..codes])))  # ensure we only get codes specified
    
    # print(multidim_arr[[s_j, r_i, m_j, "weight"]]) is getting correct weight 
    g_w_vec = multidim_arr[[s_j, r_i, m_j, "weight"]] * unweighted
    
    g_w_vecs[[g_w_vecs_counter]] <- g_w_vec
    g_w_vecs_counter = g_w_vecs_counter + 1 
  }
  
  return(g_w_vecs)
}

# 0 out diagonal of a square matrix
f_diag <- function(
    mat # a square matrix
) {
  if (!is.matrix(mat) || nrow(mat) != ncol(mat)) {
    stop("Input must be a square matrix.")
  }
  
  
  diag(mat) <- 0
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
#' @param context_model TMA context model containing contexts for each unit of analysis
#' @param decay Which decay function to use. By default, this is a simple window 
#' @param calculate_adj_vectors_using How to compute adj vectors. By default, this is matmul(g^T, r).
#' @param adj_vec_transform How to transform adj. vecs e.g. binarization. By default, this is empty  
#' @param accumulate_unit_vectors_by How to compute final adjacency vector from adjacency vectors
#' accross all of a unit's utterances; by default, this is colSums 
#' @param normalization How to normalize. Can be \ell_2 or NA
#' @param summarize_ground_using How to combine ground into one vec. By default, this is colSums
#' @param return_ena_set Whether or not to return an ENA set. By default, this is ONA
#' 
#' @return T/ONA (or T/ENA) accumulation
#@export
accum_multidim <- function(
    multidim_arr, 
    unique_senders,
    unique_receivers,
    unique_modes,
    sender_cols,
    receiver_cols,
    time_column,
    mode_column,
    codes,
    context_model,
    # Nothing below this line is implemented in terms of arguments
    decay = decay(simple_window),
    calculate_adj_vectors_using = inner_product_ground_response,
    adj_vec_transform = NA,
    accumulate_unit_vectors_by = colSums,
    norm_by = "No",
    summarize_ground_using = colSums,
    return_ena_set = FALSE,
    ...
) {
  # Preprocess multidim array
  result <- preprocess(multidim_arr, unique_senders, unique_receivers, unique_modes, 
                       sender_cols, receiver_cols, mode_column)
  multidim_arr <- result$multidim_arr
  sender_cols <- result$sender_cols
  receiver_cols <- result$receiver_cols
  mode_column <- result$mode_column
  
  # print(multidim_arr[,,,"weight"]) # working 
  # print(multidim_arr[,,,"window"]) # working
  
  adj_vectors <- list()
  units <- context_model[["model"]][["unit.labels"]] # assuming TMA generating contexts
  # For each unit
  for (unit in units) {
   # if (as.character(unit) == as.character(units[[1]]) | (as.character(unit) == (as.character(units[[3]])))) {
      #next
    #}
    # List to store adjacency vectors for this unit
    adj_vector <- list()
    
    # Ensure context has column "standin" with values "standin" for all lines
    context <- context_model[["model"]][["contexts"]][[unit]]
    context[,"standin"] = "standin"
    
    # List to store connection matrices before accumulation for this unit
    counting_matrices <- list()
    counting_matrices_counter = 1
    
    # For each context, get unit's lines in that context (r_vec)
    for (i in seq(1, nrow(context))) { 
      r_vec = context[i,] 
      # If this response vector isn't our units, skip
      if (r_vec$QEUNIT == unit) {
        # Get all lines before this line, which are individual g_vec, and only get those that project
        # far enough
        g_vecs <- apply_windows(multidim_arr, sender_cols, receiver_cols, time_column, mode_column, 
                                i, context, codes)
        
        if (length(g_vecs) == 0) {
          temp <- r_vec
          temp[,codes] <- 0
          g_vecs[[1]] <- temp
         #  print(g_vecs)
        }
      

        # Query multidimensional array to reweight g^w_vec = w^{srm} * g_vec
        # note: looks to be working for no sender, no receiver, modality
        g_w_vecs <- apply_weights(multidim_arr, sender_cols, receiver_cols, mode_column, codes, i, context,
                                  g_vecs) 

        
        # Sum all g^w_vec together as g^{ws}_vec
        g_ws_vec <- Reduce("+", g_w_vecs)
  
        # Compute counting matrix = g^{ws}_vec r_vec^T + 0.5w_{resp}^{srm} * f_diag(r_vec r_vec^T)
        s_i <- r_vec[[sender_cols]]
        r_i <- r_vec[[receiver_cols]]
        m_i <- r_vec[[mode_column]]
        w_rr = multidim_arr[s_i, r_i, m_i, "weight"]
        r_vec <- as.numeric(unname(c(r_vec[,..codes])))
        g_ws_vec <- as.numeric(g_ws_vec)
        # print(g_ws_vec)
        # print(r_vec)
        # print((g_ws_vec %*% t(r_vec))) 
        # print(f_diag(r_vec %*% t(r_vec))) 
        # print(0.5 * w_rr * f_diag(r_vec %*% t(r_vec)))
        counting_matrix = (g_ws_vec %*% t(r_vec)) + 0.5 * w_rr * f_diag(r_vec %*% t(r_vec))
        counting_matrices[[counting_matrices_counter]] = counting_matrix
        
        counting_matrices_counter = counting_matrices_counter + 1
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
    adj_vector_names <- paste(row_col_combinations[,1], "&", row_col_combinations[,2])
    
    adj_vector <- omega_matrix[cbind(match(row_col_combinations[, 1], rownames(omega_matrix)), 
                                     match(row_col_combinations[, 2], colnames(omega_matrix)))]
    
    adj_vector <- setNames(adj_vector, adj_vector_names)
    
    # Apply normalization 
    if (norm_by == "ell_2") {
      l2_adj_vector <- sqrt(sum(adj_vector^2))
      normed_adj_vector <- adj_vector / l2_adj_vector
    } else {
      normed_adj_vector <- adj_vector
    }
    
    # Save unit's adjacency vec
    adj_vectors[[unit]] <- normed_adj_vector
  }
  
  # Return accumulation (adjacency vecs for all units)
  return(adj_vectors)
}
