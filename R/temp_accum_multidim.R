get_unique_values <- function(df, sender_cols, receiver_cols, mode_column) {
  # Extract unique values for each sender column
  unique_senders <- lapply(sender_cols, function(col) unique(df[[col]]))
  names(unique_senders) <- sender_cols
  
  # Extract unique values for each receiver column
  unique_receivers <- lapply(receiver_cols, function(col) unique(df[[col]]))
  names(unique_receivers) <- receiver_cols
  
  # Extract unique values for the mode column
  unique_modes <- unique(df[[mode_column]])
  
  return(list(
    unique_senders = unique_senders,
    unique_receivers = unique_receivers,
    unique_modes = unique_modes
  ))
}


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
  
  # Construct dimensions dynamically
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
    
    old_index_list <- lapply(old_index_list, function(x) ifelse(is.na(x), "standin", x))
    
    weight_new_index_list <- c(new_index_list, "weight")
    # print(weight_new_index_list)
    window_new_index_list <- c(new_index_list, "window")
    
    # print(matrix(weight_new_index_list, 1))
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




apply_windows <- function(
    multidim_arr, 
    sender_cols,
    receiver_cols,
    time_column,
    mode_column,
    i, 
    context, 
    codes 
) {
  g_vecs <- list()
  g_vecs_counter = 1
  
  r_i = c()
  
  for (k in 1:length(receiver_cols)) {
    current_col <- receiver_cols[[k]]
    r_i <- c(r_i, pull(context[i, ..current_col]))
  }
  
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
    
    val_to_check <- context[j, ..time_column] + multidim_arr[matrix(c(s_j, r_i, m_j, "window"), 1)]
    if (val_to_check >= context[i, ..time_column]) {
      g_vecs[[g_vecs_counter]] <- context[j,]
      g_vecs_counter = g_vecs_counter + 1 
    }
  }
  
  return(g_vecs)
}

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
    decay = decay(simple_window),
    calculate_adj_vectors_using = inner_product_ground_response,
    adj_vec_transform = NA,
    accumulate_unit_vectors_by = colSums,
    norm_by = "No",
    summarize_ground_using = colSums,
    return_ena_set = FALSE,
    ...
) {
  result <- preprocess(multidim_arr, unique_senders, unique_receivers, unique_modes, 
                       sender_cols, receiver_cols, mode_column)
  multidim_arr <- result$multidim_arr
  sender_cols <- result$sender_cols
  receiver_cols <- result$receiver_cols
  mode_column <- result$mode_column
  
  # print(multidim_arr)
  
  adj_vectors <- list()
  units <- context_model[["model"]][["unit.labels"]]
  for (unit in units) {
    adj_vector <- list()
    
    context <- context_model[["model"]][["contexts"]][[unit]]
    context[,"standin"] = "standin"
    
    counting_matrices <- list()
    counting_matrices_counter = 1
    
    for (i in seq(1, nrow(context))) { 
      r_vec = context[i,] 
      if (r_vec$QEUNIT == unit) {
        g_vecs <- apply_windows(multidim_arr, sender_cols, receiver_cols, time_column, mode_column, 
                                i, context, codes)
        
        if (length(g_vecs) == 0) {
          temp <- r_vec
          temp[, codes] <- 0
          g_vecs[[1]] <- temp
        }
        
        g_w_vecs <- apply_weights(multidim_arr, sender_cols, receiver_cols, mode_column, codes, i, context,
                                  g_vecs) 
        
        g_ws_vec <- Reduce("+", g_w_vecs)
        
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
        
        counting_matrix = (g_ws_vec %*% t(r_vec)) + 0.5 * w_rr * f_diag(r_vec %*% t(r_vec))
        counting_matrices[[counting_matrices_counter]] = counting_matrix
        
        counting_matrices_counter = counting_matrices_counter + 1
      }
    }
    
    omega_matrix = Reduce("+", counting_matrices)
    rownames(omega_matrix) = codes
    colnames(omega_matrix) = codes
    
    row_col_combinations <- expand.grid(rownames(omega_matrix), colnames(omega_matrix))
    adj_vector_names <- paste(row_col_combinations[,1], "&", row_col_combinations[,2])
    
    adj_vector <- omega_matrix[cbind(match(row_col_combinations[, 1], rownames(omega_matrix)), 
                                     match(row_col_combinations[, 2], colnames(omega_matrix)))]
    
    adj_vector <- setNames(adj_vector, adj_vector_names)
    
    if (norm_by == "ell_2") {
      l2_adj_vector <- sqrt(sum(adj_vector^2))
      normed_adj_vector <- adj_vector / l2_adj_vector
    } else {
      normed_adj_vector <- adj_vector
    }
    
    adj_vectors[[unit]] <- normed_adj_vector
  }
  
  return(adj_vectors)
}
