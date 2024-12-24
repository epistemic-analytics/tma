# Pull unique sender, receiver, and modality values from a dataframe
get_unique_values <- function(df, sender_cols, receiver_cols, mode_column) {
  # Extract unique values for each sender column
  unique_senders <- lapply(sender_cols, function(col) unique(df[[col]]))
  names(unique_senders) <- sender_cols
  
  # Extract unique values for each receiver column
  unique_receivers <- lapply(receiver_cols, function(col) unique(df[[col]]))
  names(unique_receivers) <- receiver_cols
  
  # Extract unique values for the mode column
  if (length(mode_column) == 0) {
    unique_modes <- list()
  } else {
    unique_modes <- unique(df[[mode_column]])
  }
  
  return(list(
    unique_senders = unique_senders,
    unique_receivers = unique_receivers,
    unique_modes = unique_modes
  ))
}


# Function to generate multidim arr
make_multidim_arr <- function(df, 
                  sender_cols, 
                  receiver_cols, 
                  mode_column, 
                  default_window,
                  default_weight) {
  
  unique_list = get_unique_values(df, sender_cols, receiver_cols, mode_column)
  weight_window_axis = c("weight", "window")
  
  # Pull out unique senders, receivers, and modes from unique_list
  unique_senders <- unique_list$unique_senders
  unique_receivers <- unique_list$unique_receivers
  unique_modes <- unique_list$unique_modes
  
  # Set dimension indices to unique values
  dim_names <- list()
  for (i in seq_along(sender_cols)) {
    dim_names[[paste0("sender_", i)]] <- unique_senders[[i]]
  }
  for (i in seq_along(receiver_cols)) {
    dim_names[[paste0("receiver_", i)]] <- unique_receivers[[i]]
  }
  if (length(unique_modes) > 0) {
    dim_names[["modes"]] <- unique_modes
  }
  dim_names[["weight_window"]] <- weight_window_axis
  
  # Set dims accordingly s.t. our array is [S, R, M, weight/window]
  dims <- c()
  for (i in seq_along(sender_cols)) {
    dims <- c(dims, length(unique_senders[[i]]))
  }
  for (i in seq_along(receiver_cols)) {
    dims <- c(dims, length(unique_receivers[[i]]))
  }
  if (length(unique_modes > 0)) { 
    dims <- c(dims, length(unique_modes), length(weight_window_axis))
  } else {
    dims <- c(dims, length(weight_window_axis))
  }
  

  # Init multidimensional array; include sender, receiver, mode columns; default weight; default window as attributes 
  multidim_arr <- array(0, 
                        dim = dims, 
                        dimnames = dim_names)
  
  attr(multidim_arr, "sender_cols") <- sender_cols
  attr(multidim_arr, "receiver_cols") <- receiver_cols
  attr(multidim_arr, "mode_column") <- mode_column
  attr(multidim_arr, "default_window") <- default_window
  attr(multidim_arr, "default_weight") <- default_weight
  
  # print(multidim_arr)
  
  ## Ensure default window and default weight in multidim_arr 
  
  combinations <- expand.grid(
    senders = if (length(unique_senders) > 0) {
      do.call(paste, c(expand.grid(unique_senders), sep = "_"))
    } else {
      "None"  # Placeholder for empty senders
    },
    receivers = if (length(unique_receivers) > 0) {
      do.call(paste, c(expand.grid(unique_receivers), sep = "_"))
    } else {
      "None"  # Placeholder for empty receivers
    },
    modes = if (length(unique_modes) > 0) { unique_modes 
    } else { 
       "None" # Placeholder for empty modes 
    },
    stringsAsFactors = FALSE
  )
  
  
  # remove NAs
  combinations <- unique(combinations[rowSums(is.na(combinations)) < ncol(combinations), ])
  
  # print(combinations)

  
  for (i in seq_len(nrow(combinations))) {
    sender <- combinations$senders[i]
    receiver <- combinations$receivers[i]
    modality <- combinations$modes[i]
    
    sender_parts <- if (sender == "None") character(0) else strsplit(sender, "_")[[1]]
    receiver_parts <- if (receiver == "None") character(0) else strsplit(receiver, "_")[[1]]
    modality_parts <- if (modality == "None") character(0) else modality
    index_list <- c(sender_parts, receiver_parts, modality_parts)

    weight_new_index_list <- c(index_list, "weight")
    window_new_index_list <- c(index_list, "window")

    multidim_arr[matrix(as.vector(weight_new_index_list), 1)] <- default_weight
    multidim_arr[matrix(as.vector(window_new_index_list), 1)] <- default_window
  }
  
  return(multidim_arr)
}



# If sender_cols, receiver_cols, or mode_column are empty, have our standin. Be sure to fill in 
# standin as expected in multidim_arr. 
# We do this so we don't fail when they don't specify senders/receivers/mode
preprocess <- function(
    df, 
    multidim_arr
) {
  # Get sender, receiver, and mode columns + unique values from multidim_arr
  sender_cols <- attr(multidim_arr, "sender_cols")
  receiver_cols <- attr(multidim_arr, "receiver_cols")
  mode_column <- attr(multidim_arr, "mode_column")
  
  unique_list = get_unique_values(df, sender_cols, receiver_cols, mode_column)
  # Pull out unique senders, receivers, and modes from unique_list
  unique_senders <- unique_list$unique_senders
  unique_receivers <- unique_list$unique_receivers
  unique_modes <- unique_list$unique_modes
  
  old_unique_senders <- unique_senders
  old_unique_receivers <- unique_receivers
  old_unique_modes <- unique_modes
  if (length(old_unique_senders) == 0) {
    old_unique_senders <- NA  # Placeholder for missing senders
  }
  if (length(old_unique_receivers) == 0) {
    old_unique_receivers <- NA  # Placeholder for missing receivers
  }
  if (length(old_unique_modes) == 0) {
    old_unique_modes <- NA # Placeholder for missing modes
  }
  
  # Ensure standin is present for any empty columns
  if (length(sender_cols) == 0) {
    sender_cols <- "standin"
    unique_senders <- list(standin = "standin")
  }
  if (length(receiver_cols) == 0) {
    receiver_cols <- "standin"
    unique_receivers <- list(standin = "standin")
  }
  if (length(mode_column) == 0) {
    mode_column <- "standin"
    unique_modes <- "standin"
  }
  
  # Create a new multidimensional array to hold expanded values
  weight_window_axis = c("weight", "window")
  
  # Construct dimension names dynamically
  dim_names <- list()
  for (i in seq_along(sender_cols)) {
    dim_names[[paste0("sender_", i)]] <- unique_senders[[i]]
  }
  for (i in seq_along(receiver_cols)) {
    dim_names[[paste0("receiver_", i)]] <- unique_receivers[[i]]
  }
  dim_names[["modes"]] <- unique_modes
  dim_names[["weight_window"]] <- weight_window_axis
  
  dims <- c()
  for (i in seq_along(sender_cols)) {
    dims <- c(dims, length(unique_senders[[i]]))
  }
  for (i in seq_along(receiver_cols)) {
    dims <- c(dims, length(unique_receivers[[i]]))
  }
  dims <- c(dims, length(unique_modes), length(weight_window_axis))
  
  new_multidim_arr <- array(0, dim = dims, dimnames = dim_names)
  
  combinations <- expand.grid(
    new_senders = do.call(paste, c(expand.grid(unique_senders), sep = "_")),
    new_receivers = do.call(paste, c(expand.grid(unique_receivers), sep = "_")),
    new_modes = unique_modes,
    old_senders = do.call(paste, c(expand.grid(old_unique_senders), sep = "_")),
    old_receivers = do.call(paste, c(expand.grid(old_unique_receivers), sep = "_")),
    old_modes = old_unique_modes,
    stringsAsFactors = FALSE
  )
  
  combinations <- combinations[
    (# just modality
      (combinations$new_senders == "standin" & combinations$new_receivers == "standin" & combinations$new_modes == combinations$old_modes) | 
        # just sender
        (combinations$new_senders == combinations$old_senders & combinations$new_receivers == "standin" & combinations$new_modes == "standin") | 
        # just receiver
        (combinations$new_senders == "standin" & combinations$new_receivers == combinations$old_receivers & combinations$new_modes == "standin") | 
        # modality + sender
        (combinations$new_senders ==  combinations$old_senders & combinations$new_receivers == "standin" & combinations$new_modes == combinations$old_modes) | 
        # modality + receiver
        (combinations$new_senders == "standin" & combinations$new_receivers == combinations$old_receivers & combinations$new_modes == combinations$old_modes) | 
        # sender + receiver
        (combinations$new_senders == combinations$old_senders & combinations$new_receivers == combinations$old_receivers & combinations$new_modes == "standin") | 
        # modality + sender + receiver
        (combinations$new_senders == combinations$old_senders & combinations$new_receivers == combinations$old_receivers & combinations$new_modes == combinations$old_modes)) 
    ,
  ]
  
  # remove NA rows
  combinations <- combinations[rowSums(is.na(combinations)) < ncol(combinations), ]
  
  for (i in seq_len(nrow(combinations))) {
    new_s <- combinations$new_senders[i]
    new_r <- combinations$new_receivers[i]
    new_m <- combinations$new_modes[i]
    orig_s <- combinations$old_senders[i]
    orig_r <- combinations$old_receivers[i]
    orig_m <- combinations$old_modes[i]
    
    new_index_list <- c(strsplit(new_s, "_")[[1]], strsplit(new_r, "_")[[1]], new_m)
    old_index_list <- c(strsplit(orig_s, "_")[[1]], strsplit(orig_r, "_")[[1]], orig_m)
    old_index_list <- old_index_list[!is.na(old_index_list) & old_index_list != "NA"]
    
    weight_new_index_list <- c(new_index_list, "weight")
    window_new_index_list <- c(new_index_list, "window")
    
    new_multidim_arr[matrix(as.vector(weight_new_index_list), 1)] <- do.call(`[`, c(list(multidim_arr), as.list(old_index_list), list("weight")))
    new_multidim_arr[matrix(as.vector(window_new_index_list), 1)] <- do.call(`[`, c(list(multidim_arr), as.list(old_index_list), list("window")))
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
# This should return the actual ground vectors from the context i.e. not just the codes
apply_windows <- function(
    multidim_arr, 
    sender_cols,
    receiver_cols,
    time_column,
    duration_column,
    mode_column,
    i, 
    context, 
    codes 
) {
  g_vecs <- list()
  g_vecs_counter = 1
  
  # ground is at j, response is at i
  r_i = c()
  for (k in 1:length(receiver_cols)) {
    current_col <- receiver_cols[[k]]
    r_i <- c(r_i, pull(context[i, ..current_col]))
  }
  
  # first line is response line--since we aren't including response lines, just provide a dummy vec w/ 0s @ codes
  if (i == 1) { 
    temp_vec <- context[i,]
    temp_vec[, codes] <- 0
    g_vecs[[g_vecs_counter]] <- temp_vec
    return(g_vecs)
  }
  
  for (j in seq(1, i-1)) {
    s_j <- c()
    for (k in 1:length(sender_cols)) {
      current_col <- sender_cols[[k]]
      s_j <- c(s_j, pull(context[j, ..current_col]))
    }
    m_j = as.vector(unlist(context[j, ..mode_column]))
    
    # Verify t_i + w_{s_j, r_i, m_j} \geq t_j
    if (length(duration_column) == 0) {
      val_to_check <- context[j, ..time_column] + multidim_arr[matrix(c(s_j, r_i, m_j, "window"), 1)]
      if (val_to_check >= context[i, ..time_column]) {
        g_vecs[[g_vecs_counter]] <- context[j,]
        g_vecs_counter = g_vecs_counter + 1 
      }
    } else {
      val_to_check <- context[j, ..time_column] + context[j, ..duration_column] + multidim_arr[matrix(c(s_j, r_i, m_j, "window"), 1)]
      if (val_to_check >= (context[i, ..time_column] + context[j, ..duration_column])) {
        g_vecs[[g_vecs_counter]] <- context[j,]
        g_vecs_counter = g_vecs_counter + 1 
      }
    }
  }
  
  return(g_vecs)
}

# Applies weights to ground vectors for a response line 
# This should return the weighted ground vectors with just the codes
apply_weights <- function(
    multidim_arr, 
    sender_cols,
    receiver_cols,
    mode_column,
    codes,
    i, 
    context,
    g_vecs 
) {
  g_w_vecs <- list()
  g_w_vecs_counter = 1 
  # ground is at j, response is at i
  r_i <- c()
  for (k in 1:length(receiver_cols)) {
    current_col <- receiver_cols[[k]]
    r_i <- c(r_i, pull(context[i, ..current_col]))
  }
  for (vec in g_vecs) {
    s_j <- c()
    for (k in 1:length(sender_cols)) {
      current_col <- sender_cols[[k]]
      s_j <- c(s_j, pull(context[vec, ..current_col]))
    }
    m_j = as.vector(unlist(vec[, ..mode_column]))
    
    # ensure we only get codes specified
    unweighted <- as.numeric(unname(c(vec[, ..codes])))  
    
    g_w_vec = multidim_arr[matrix(c(s_j, r_i, m_j, "weight"), 1)] * unweighted
    
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
#' [sender, receiver, modality, weight/window] (and will be b/c of how we initialize it above)
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
    df, # original df; needed to get unique_senders, unique_receivers, unique_modes in preprocess
    multidim_arr, 
    time_column,
    codes, 
    context_model,
    duration_column = list(), # TODO: implement
    norm_by = "No", # TODO: implement l1 normalization
    return_ena_set = FALSE,
    ...
) {
  result <- preprocess(df, multidim_arr)
  
  multidim_arr <- result$multidim_arr
  sender_cols <- result$sender_cols
  receiver_cols <- result$receiver_cols
  mode_column <- result$mode_column
  
  # print(multidim_arr)
  
  adj_vectors <- list()
  units <- context_model[["model"]][["unit.labels"]]
  for (unit in units) {
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
        # Get all lines *strictly* before this line, which are individual g_vec, and only get those that project
        # far enough
        g_vecs <- apply_windows(multidim_arr, sender_cols, receiver_cols, time_column, duration_column, mode_column, 
                                i, context, codes)
        
        if (length(g_vecs) == 0) {
          temp <- r_vec
          temp[, codes] <- 0
          g_vecs[[1]] <- temp
        }
        
        # Query multidimensional array to reweight g^w_vec = w^{srm} * g_vec
        g_w_vecs <- apply_weights(multidim_arr, sender_cols, receiver_cols, mode_column, codes, i, context,
                                  g_vecs) 
        
        # Sum all g^w_vec together as g^{ws}_vec
        g_ws_vec <- Reduce("+", g_w_vecs)
        
        # Compute counting matrix = g^{ws}_vec r_vec^T + 0.5w_{resp}^{srm} * f_diag(r_vec r_vec^T)
        s_i <- c()
        for (k in 1:length(sender_cols)) {
          current_col <- sender_cols[[k]]
          s_i <- c(s_i, pull(context[i, ..current_col]))
        }
        r_i <- c()
        for (k in 1:length(receiver_cols)) {
          current_col <- receiver_cols[[k]]
          r_i <- c(r_i, pull(context[i, ..current_col]))
        }
        m_i <- as.vector(unlist(context[i, ..mode_column]))
        w_rr = multidim_arr[matrix(c(s_i, r_i, m_i, "weight"), 1)]
        r_vec <- as.numeric(unname(c(r_vec[, ..codes])))
        g_ws_vec <- as.numeric(g_ws_vec)
        
        if (return_ena_set == FALSE) { # ONA 
          counting_matrix = (g_ws_vec %*% t(r_vec)) + 0.5 * w_rr * f_diag(r_vec %*% t(r_vec))
        } else { # ENA (w_rr * rr^T + (qr^T + rq^T))
          counting_matrix = w_rr * (r_vec %*% t(r_vec)) + (g_ws_vec %*% t(r_vec) + r_vec %*% t(g_ws_vec)) 
        }
        
        counting_matrices[[counting_matrices_counter]] = counting_matrix
        
        counting_matrices_counter = counting_matrices_counter + 1
      }
    }
    
    # Sum all counting matrices to get Omega matrix 
    omega_matrix = Reduce("+", counting_matrices)
    rownames(omega_matrix) = codes
    colnames(omega_matrix) = codes
    
    if (return_ena_set == FALSE) { # ONA
      # Make into adjacency vector
      row_col_combinations <- expand.grid(rownames(omega_matrix), colnames(omega_matrix))
      adj_vector_names <- paste(row_col_combinations[,1], "&", row_col_combinations[,2])
      
      adj_vector <- omega_matrix[cbind(match(row_col_combinations[, 1], rownames(omega_matrix)), 
                                       match(row_col_combinations[, 2], colnames(omega_matrix)))]
      
      adj_vector <- setNames(adj_vector, adj_vector_names)
    } else { 
      upper_indices <- which(upper.tri(omega_matrix, diag = FALSE)) 
      adj_vector <- omega_matrix[upper_indices]
      upper_rows <- rownames(omega_matrix)[row(omega_matrix)[upper_indices]]
      upper_cols <- colnames(omega_matrix)[col(omega_matrix)[upper_indices]]
      adj_vector_names <- paste(upper_rows, "&", upper_cols)
      
      adj_vector <- setNames(adj_vector, adj_vector_names)
    }
  
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
