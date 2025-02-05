// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

//' @title n choose 2
//' @param n int
//' @export
// [[Rcpp::export]]
int choose_two(int n) {
  return (n * (n - 1)) / 2;
}

arma::rowvec vector_to_summed_uppertri(arma::vec blank) {
  // Rcpp::Rcout << "V1" << std::endl;
  // Rcpp::Rcout << blank << std::endl;
  int code_count = pow(blank.size(), 0.5);
  int tri_size = choose_two(code_count);
  arma::rowvec network_vector = arma::rowvec(tri_size);
  // Rcpp::Rcout << "V2" << std::endl;
  // Rcpp::Rcout << network_vector << std::endl;
  
  arma::mat blank_mat(blank);
  blank_mat.reshape(code_count, code_count);
  // Rcpp::Rcout << "Mat" << std::endl;
  // Rcpp::Rcout << blank_mat;
  
  blank_mat = blank_mat + blank_mat.t();
  arma::colvec blank_vec = blank_mat.as_col();
  // Rcpp::Rcout << "V3" << std::endl;
  arma::uvec upper_indices = arma::trimatu_ind( arma::size(blank_mat), 1 );
  // Rcpp::Rcout << "V4" << std::endl;
  network_vector = arma::conv_to< arma::rowvec >::from(blank_vec.elem(upper_indices));
  // network_vector = blank_vec.elem(upper_indices);
  // Rcpp::Rcout << "V5" << std::endl;
  
  return network_vector;
}

// std::vector<double> accumulate_network(
// [[Rcpp::export]]
Rcpp::List accumulate_network(
  std::string unit,
  SEXP unit_rows_sexp, 
  Rcpp::NumericVector code_cols,
  Rcpp::Function decay_function,
  int time_col = -1,
  bool ordered = true
) {
  // Rcpp::Rcout << "1" << std::endl;
  arma::vec blank(code_cols.size() * code_cols.size());
  Rcpp::DataFrame unit_rows = Rcpp::as<Rcpp::DataFrame>(Rcpp::wrap(unit_rows_sexp));
  Rcpp::NumericMatrix codes(unit_rows.nrow(), code_cols.size());

  Rcpp::CharacterVector qeunit_col = unit_rows["QEUNIT"];
  Rcpp::NumericVector qeunit_row_indices;
  
  for(int i = 0; i < qeunit_col.size(); i++) {
    if(qeunit_col[i] == unit) {
      qeunit_row_indices.push_back(i);
    }
  }
  
  arma::mat individual_networks(qeunit_row_indices.size(), (code_cols.size() * code_cols.size()) );
  for(int i = 0; i < code_cols.size(); i++) {
    Rcpp::NumericVector     unit_code_col = unit_rows[code_cols[i]];
    Rcpp::NumericMatrix::Column codes_col = codes(Rcpp::_, i);
    codes_col = unit_code_col;
  }

  // Environment decay_fun_env(decay_function);
  Rcpp::Environment decay_fun_env = decay_function.environment();
  
  // Rcpp::Rcout << "3" << std::endl;
  for(int i = 0; i < qeunit_row_indices.size(); i++) {
    int unit_response_row = qeunit_row_indices[i];
    
    arma::uvec ground_rows = arma::regspace<arma::uvec>(0, 1, unit_response_row);
    Rcpp::NumericVector ground_rows2 = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(ground_rows));
    Rcpp::NumericVector ground_dist_from_response(ground_rows.size());
    if (time_col == -1) {
      ground_dist_from_response = Rcpp::seq(0, ground_rows.size()-1);
      // std::reverse(ground_dist_from_response.begin(), ground_dist_from_response.end());
    }
    else {
      arma::uvec times = Rcpp::as<arma::uvec>(wrap(unit_rows[time_col]));
      arma::uvec ground_times = times.elem(ground_rows);
      arma::uword response_time = ground_times[ground_times.size() - 1];
      arma::uvec response_times(ground_times.n_elem, arma::fill::value(response_time));
      
      ground_dist_from_response = response_times - ground_times;
    }
    
    decay_fun_env["UNIT"] = unit;
    decay_fun_env["RESPONSE_INDEX"] = unit_response_row + 1;
    // Rcpp::Rcout << "Rows: " << (ground_rows + 1) << std::endl;
    decay_fun_env["ROWS"] = (ground_rows + 1);
    
    Rcpp::List myList(unit_rows.size());
    Rcpp::CharacterVector namevec;
    std::string namestem = "Column Heading ";
    for (int i=0; i < unit_rows.ncol(); i++) {
      // arma::uvec fnd = arma::find(ground_rows == i);
      // Rcpp::Rcout << "Fnd: " << fnd.size() << std::endl;
      // if(fnd.size() > 0) {
        // Rcpp::Rcout << "i: " << i << std::endl;
        Rcpp::CharacterVector v = unit_rows(i);
        myList[i] = v[ground_rows2]; // adding vectors
        // Rcpp::Rcout << "V : " << v << std::endl;
        // myList[i] = myList
        // namevec.push_back(namestem+std::string(1,(char)(((int)'a') + i))); // making up column names
      // }
    }
    myList.attr("names") = unit_rows.attr("names");
    Rcpp::DataFrame dfout(myList);
    decay_fun_env["FULL_CONTEXT"] = dfout;
    // arma::uvec all_rows(ground_rows.size() + 1);
    // for(int z = 0; z < ground_rows.size(); z++) {
    //   all_rows[z] = ground_rows[z] + 1;
    // }
    // // all_rows[all_rows.size() - 1] = unit_response_row + 2;
    
    arma::vec decay_effect = Rcpp::as<arma::vec>(Rcpp::wrap(decay_function(ground_dist_from_response)));

    Rcpp::NumericMatrix ground_codes_raw = codes( Rcpp::Range( ground_rows[0], ground_rows[ground_rows.size() - 1]  ), Rcpp::_);
    
    arma::mat ground_codes_mat = Rcpp::as<arma::mat>(ground_codes_raw);
    arma::mat codes_decayed(ground_codes_mat.n_rows, ground_codes_mat.n_cols);
    for(int j = 0; j < codes_decayed.n_rows; j ++) {
      codes_decayed.row(j) = ground_codes_mat.row(j) * decay_effect[j];
    }

    arma::rowvec response_row = codes_decayed.tail_rows(1);
    arma::rowvec ground_summarized = arma::sum(codes_decayed);
    
    arma::rowvec ground_summarized_no_resp = ground_summarized - response_row;
    arma::mat connection_matrix = ground_summarized_no_resp.t() * response_row;
    
    arma::mat self_connection_matrix = response_row.t() * response_row;
    self_connection_matrix.diag() *= 0;
    self_connection_matrix = self_connection_matrix * 0.5;
    
    arma::mat full_connection_matrix = connection_matrix + self_connection_matrix;
    arma::vec full_connection_vec = arma::vectorise(full_connection_matrix);
    
    individual_networks.row(i) = full_connection_vec.t();
    blank = blank + full_connection_vec;
  }

  Rcpp::NumericMatrix individual_networks_nm = Rcpp::as<Rcpp::NumericMatrix>(Rcpp::wrap(individual_networks));
  
  Rcpp::Function subset("[.data.frame");
  individual_networks_nm.attr("unit_rows") = subset(unit_rows, qeunit_row_indices + 1, R_MissingArg);
  Rcpp::CharacterVector classes = {"unit.rows","list"};
  individual_networks_nm.attr("class") = classes;
 
  arma::rowvec network_vector;
  if(ordered == false) {
    network_vector = vector_to_summed_uppertri(blank);
  }
  else {
    network_vector = blank.t();
  }
  // Rcpp::Rcout << "5" << std::endl;
  
  return Rcpp::List::create(
    Rcpp::Named("networks") = Rcpp::as<std::vector<double>>(Rcpp::wrap(network_vector)),
    Rcpp::Named("row_networks") = individual_networks_nm
  );
}

//' fast accumulate networks
//' @param x TBD
//' @param code_cols TBD
//' @param decay_function TBD
//' @param time_col TBD
//' @param ordered TBD
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List accumulate_networks(
    Rcpp::List x,
    Rcpp::NumericVector code_cols,
    Rcpp::Function decay_function,
  int time_col = -1,
  bool ordered = true
) {
  Rcpp::List model = x["model"];
  Rcpp::List contexts = model["contexts"];
  Rcpp::CharacterVector units = model["unit.labels"];
  
  // Rcpp::Rcout << "N codes: " << code_cols.size() << std::endl;
  int ncol = code_cols.size() * code_cols.size();
  if(ordered == false) {
    ncol = choose_two(code_cols.size());
    // Rcpp::Rcout << "Ncol: " << ncol << std::endl;
  }
  Rcpp::NumericMatrix blank(units.size(), ncol);
  Rcpp::List unit_networks_by_row = Rcpp::List::create();
  
  for(int i = 0; i < units.size(); i++) {
    Rcpp::NumericMatrix::Row unit_row = blank.row(i);
    std::string unit_name = Rcpp::as<std::string>(Rcpp::wrap(units[i]));
    
    Rcpp::List accumulated = accumulate_network(unit_name, contexts[unit_name], code_cols, decay_function, time_col, ordered);
    Rcpp::NumericVector unit_network_conv = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(accumulated["networks"]));
    Rcpp::NumericMatrix row_networks = accumulated["row_networks"];
    
    unit_networks_by_row[unit_name] = row_networks;
    unit_row = unit_network_conv;
  }
  
  return Rcpp::List::create(
    Rcpp::Named("networks") = blank,
    Rcpp::Named("networks_by_row") = unit_networks_by_row
  );
}

//' @export
// [[Rcpp::export]]
arma::mat calculate_adjacency_matrix(arma::rowvec ground, arma::rowvec response, double response_weight) {
 arma::mat response_matrix = response.t() * response;
 response_matrix.diag().zeros();
 
 arma::mat ground_by_response = ground.t() * response;
 
 return ground_by_response + (0.5 * response_weight * response_matrix);
}

//' @export
// [[Rcpp::export]]
arma::rowvec adjacency_matrix_to_vector(arma::mat x, bool full = true) {
 if(full == true) {
   return arma::vectorise(x).t();
 }
 else {
   arma::mat combined = arma::trimatu(x, 1) + arma::trimatl(x, -1);
   arma::uvec combined_tri_inds = arma::trimatu_ind(arma::size(combined), 1); 
   return arma::vectorise(combined(combined_tri_inds)).t();
 }
}

std::vector<std::vector<int>> matrix_to_2d_vector(Rcpp::NumericMatrix mat) {
  int n = mat.nrow();
  std::vector<std::vector<int>> vec(n);
  
  for (int i = 0; i < n; ++i) {
    Rcpp::NumericVector row = mat(i, Rcpp::_); // Extract the i-th row
    vec[i] = Rcpp::as<std::vector<int>>(row); // Convert the row to std::vector<int>
  }
  return vec;
}

//' @export
// [[Rcpp::export]]
int calculate_1d_index(const std::vector<int>& indices, const std::vector<int>& dims) {
  if (indices.size() != dims.size()) {
     throw std::invalid_argument("Number of indices must match number of dimensions.");
  }
 
  int index = 0;
  int stride = 1;
  for (int i = dims.size() - 1; i >= 0; --i) {
    if (indices[i] < 0 || indices[i] >= dims[i]) {
      throw std::out_of_range("Index out of range.");
    }
    index += indices[i] * stride;
    stride *= dims[i];
  }
  return index;
}


// int calculate_1d_index(const std::vector<int>& indices, const std::vector<int>& dims) {
//   if (indices.size() != dims.size()) {
//     throw std::invalid_argument("Number of indices must match number of dimensions.");
//   }
//   
//  // formula for mode-k tensor in R^{n_1 \times ... \times n_k} 
//  // idx = i_0 + \sum_{v = 1}^k [i_v * \Pi_{j = 0}^{v-1}n_j] (this is reindexed to index from 0, b/c we in C++)
//  // the below operationalizes this
//   
//   size_t linear_index = 0;
//   size_t stride = 1;
//   
//   for (size_t v = 0; v < indices.size(); ++v) {
//     linear_index += indices[v] * stride;
//     stride *= dims[v]; 
//   }
//   
//   return linear_index;
// }

void insert_element(arma::uvec& vec, double value) {
  // Add a new row at the end
  vec.insert_rows(vec.n_rows, 1);
  
  // Assign the value to the new element
  vec(vec.n_rows - 1) = value;
}


void print_vector(const std::vector<int>& row) {
  for (const auto& elem : row) {
    std::cout << elem << " ";
  }
  std::cout << std::endl;
}

void print_2d_vector(const std::vector<std::vector<int>>& vec) {
  for (const auto& row : vec) {
    print_vector(row);
  }
}

//' @export
// [[Rcpp::export]]
Rcpp::List apply_windows_d(
   Rcpp::NumericVector multidim_as_vector, // vectorized multidim arr
   Rcpp::IntegerVector dims, // colnames of senders, receivers, and modes along with weight/window
   std::vector<int> dims_sender,
   std::vector<int> dims_receiver,
   std::vector<int> dims_mode,
   Rcpp::NumericMatrix context_matrix, // matrix of this unit's context 
   std::vector<int> unit_rows, // vector of indices of unit's (response) rows in context
   arma::mat codes, // matrix of nrows(context) x |codes| 
   Rcpp::NumericVector times // vector of times of length nrows(context))
) {
  const int WINDOW_DIM = 1;
  const int WEIGHT_DIM = 0;
  
 std::vector<int> dims_v(dims.begin(), dims.end()); // vectorize dims (colnames of senders, receivers, and modes along with weight/window)  
 int code_cnt = codes.n_cols; // get # of codes 
 
 // std::cout << "Multidim: " << multidim_as_vector << std::endl;
 // std::cout << "Dimensions: "; print_vector(dims_v); 
 // std::cout << "Dims recv: "; print_vector(dims_receiver);
 
 // convert context datatable to 2D C vector
 std::vector<std::vector<int>> context_lookup = matrix_to_2d_vector(context_matrix);
 
 int index = 0; 
 // int dim_start = dims.size() - 1; // start processing dims in reverse order (not using rn)
 
 arma::mat g_w_vec(code_cnt, code_cnt, arma::fill::zeros); // initialize g_w_vec matrix as |codes| x |codes| of all 0s 
 arma::mat unit_row_connection_counts(unit_rows.size(), code_cnt * code_cnt); // initialize matrix to return of |unit's rows in context| x |codes^2| (ordered, for now)
 arma::uvec upper_indices = trimatu_ind( size(g_w_vec), 1 ); // indices of upper triangle; for ENA 
 
 // iterate over all of this unit's rows 
 for(int i = 0; i < unit_rows.size(); ++i) {
   int unit_row_i = unit_rows[i]; // get current unit row
   
   // get response line for this current unit row  
   std::vector<int> response_context_lookup = context_lookup[unit_row_i];
   arma::rowvec row_vec = codes.row(unit_row_i); // get response codes 
   double response_time = times[unit_row_i]; // get response time 
   
   
   // add a 1 to the end of response line; indicates that we are getting windows
   response_context_lookup.push_back(WINDOW_DIM); 
   
   std::vector<int> response_context_lookup_rev = response_context_lookup;
   std::reverse(response_context_lookup_rev.begin(), response_context_lookup_rev.end());
   
   // calculate_1d_index calculates index offsetted into 1d multidim_arr (vectorized); this gets idx of receiver vals, and then we get them
   int response_win_idx = calculate_1d_index(response_context_lookup_rev, dims_v); 
   int response_win = multidim_as_vector[response_win_idx];
   // std::cout << "Response context lookup reg: "; print_vector(response_context_lookup_rev);
   // std::cout << "Response context lookup rev: "; print_vector(response_context_lookup);
   // std::cout << "Response time: " << unit_row_i << " = " << response_time << std::endl;
   
   // set the end of response line (set to 1 earlier) to 0; indicates that we are getting weights
   response_context_lookup[0] = WEIGHT_DIM;
   
   int response_wgt_idx = calculate_1d_index(response_context_lookup, dims_v); // same thing as above, except for receiver weight 
   int response_weight = multidim_as_vector[response_wgt_idx];
   // std::cout << "Response context weight: "; print_vector(response_context_lookup); std::cout << " - " << response_wgt_idx << " = " << response_weight << std::endl;
   
   // vector of rows to keep 
   arma::uvec ground_rows_to_include;
   
   // vector of weights (to apply to kept rows)
   arma::colvec ground_row_weights;
   
   // get response codes, unweighted
   arma::rowvec r_vec = codes.row(unit_row_i);
   
   // proceed w/ TIF and weighting of g_vecs if it isn't the first row (otherwise, no ground)
   arma::rowvec g_ws_vec(code_cnt, arma::fill::zeros);
   arma::mat resp = calculate_adjacency_matrix(g_ws_vec, row_vec, response_weight); 
   
   if(unit_row_i > 0) {
     
     // only look at stuff from beginning of ground to the current response line 
     std::vector<std::vector<int>> context_ground = std::vector<std::vector<int>>(context_lookup.begin(), context_lookup.begin() + unit_row_i);
     
     // unfiltered g_vecs are ground rows from just before response line all the way to top of context
     for (int ground_row = unit_row_i - 1; ground_row >= 0; ground_row --) {
       double ground_row_time = times[ground_row]; // get time for this ground row (not doing select-all-times at once approach...?)
       
       std::vector<int> row_v = context_ground[ground_row]; // get actual ground row from context (using ground_row idx)
       
       
       // Set the values related to the response row to those of the response row in the ground vector; this way, we can pull all the values from 
       // multidim_arr using just the ground row 
       // Adjust array lookup to account for response row values
       row_v.push_back(WINDOW_DIM);
       
       // std::cout << "Ground context row win orig: "; print_vector(row_v);
       for (const auto& dim : dims_receiver) {
         row_v[dim] = response_context_lookup[dim];
       }
       
       // std::cout << "Ground context row win adjd: "; print_vector(row_v);
       
       std::reverse(row_v.begin(), row_v.end());
       int row_win_idx = calculate_1d_index(row_v, dims_v);
       double row_win = multidim_as_vector[row_win_idx]; // get ground window 
       
       
       double ground_row_time_adj = ground_row_time + row_win; // this computation for the TIF makes sense, I think (?)
       double diff = ground_row_time_adj - response_time;
       
       if(diff > 0) {
         // std::cout << "Ground context window: " << ground_row << " -- " << row_wgt_idx << " --> " << ground_row_time << " - " << row_win << std::endl;
         insert_element(ground_rows_to_include, ground_row); 
         

         row_v[0] = WEIGHT_DIM;

         // std::cout << "Ground context row weight: "; print_vector(row_v);
         
         // apply weights
         int row_wgt_idx = calculate_1d_index(row_v, dims_v);
         int row_wgt = multidim_as_vector[row_wgt_idx];
         
         ground_row_weights.insert_rows(ground_row_weights.n_rows, 1);
         ground_row_weights(ground_row_weights.n_rows - 1) = row_wgt;
       }
     }
     
     if(ground_rows_to_include.size() > 0) {
       arma::mat ground_codes = codes.rows(ground_rows_to_include);
       arma::mat ground_codes_by_weight = ground_codes.each_col() % ground_row_weights; // apply weights w/ hadamard (element-wise) product
       g_ws_vec = arma::sum(ground_codes_by_weight); // sum codes 
       
       // std::cout << "Ground rows summed: " << std::endl << g_ws_vec << std::endl;
     }
     else {
       // std::cout << "-- No Ground Rows Passed --" << std::endl;
       // Handle just the response row here
     }
   }
   
   // No ground available
   else {
     // std::cout << "-- No Ground on first row --" << std::endl;
   }
   
   // make g_w_vec, adjacency matrix
   // std::cout<< "using resp: " << row_vec << std::endl;
   // std::cout<< "using grnd: " << g_ws_vec << std::endl;
   // std::cout<< "using wght: " << response_weight << std::endl;
   resp = calculate_adjacency_matrix(g_ws_vec, row_vec, response_weight); 
   g_w_vec = g_w_vec + resp;
   
   // add this response line's connection counts to a matrix of |unit rows| x |connections| 
   unit_row_connection_counts.row(i) = adjacency_matrix_to_vector(resp);
 }
 
 // deallocateArray(multidim_as_array2, dims);
 
 // vectorize this unit's connection counts and return what we'd like for a single unit 
 arma::rowvec unit_connection_counts = adjacency_matrix_to_vector(g_w_vec);
 
 Rcpp::List return_list = Rcpp::List::create(
   Rcpp::Named("row_connection_counts") = unit_row_connection_counts,
   Rcpp::Named("connection_counts") = unit_connection_counts
 );
 
 return(return_list);
}

/*** R
# data(test_mockdata, package = "tma")
# mock_data <- test_mockdata;
# mock_data <- mock_data[mock_data$chatGroup == "PAM",]
# sender_cols <- c("role")
# receiver_cols <- c("role")
# mode_column <- "modality"
# 
# multidim_arr <- make_multidim_arr(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, default_window = 0, default_weight = 1)
# # multidim_arr[1:16] <- 1:16
# unit_cols <- c("userID", "condition")
# codes <- c("A", "B", "C")
# HOO_rules_model <- tma:::rules(
#   modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
# )
# context_model <- tma:::contexts(
#   x = mock_data,
#   units = unit_cols,
#   hoo_rules = HOO_rules_model
# )
# time_column = "timeStamp"
# # time_column = tma:::ATTR_NAMES$CONTEXT_COL_ID
# # result_cpp <- accum_multidim_c(context_model, multidim_arr, time_column, codes);
# #
# 

# indices <- c(2, 3, 5) # this will produce an error in C++ because the index is out of range.
# element <- rcpp_get_element(result, sizes, indices)
# print(element)

data(test_mockdata, package = "tma")
mock_data <- test_mockdata;
mock_data <- mock_data[mock_data$chatGroup == "PAM",]
sender_cols <- c("role")
receiver_cols <- c("role")
mode_column <- "modality"

multidim_arr <- make_multidim_arr(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, default_window = 20, default_weight = 1)

# set everything in multidim arr to be unique so we can understand where everything goes in vectorized multidim arr
multidim_arr["Mentor", "Mentor", "chat", "weight"] <- 0 # (0,0,0,0)
multidim_arr["Player", "Mentor", "chat", "weight"] <- 1 # (1,0,0,0)
multidim_arr["Mentor", "Player", "chat", "weight"] <- 2 # (0,1,0,0)
multidim_arr["Player", "Player", "chat", "weight"] <- 3 # (1,1,0,0)

multidim_arr["Mentor", "Mentor", "resource", "weight"] <- 4 # (0,0,1,0)
multidim_arr["Player", "Mentor", "resource", "weight"] <- 5 # (1,0,1,0)
multidim_arr["Mentor", "Player", "resource", "weight"] <- 6 # (0,1,1,0)
multidim_arr["Player", "Player", "resource", "weight"] <- 7 # (1,1,1,0)

multidim_arr["Mentor", "Mentor", "chat", "window"] <- 8 # (0,0,0,1)
multidim_arr["Player", "Mentor", "chat", "window"] <- 9 # (1,0,0,1)
multidim_arr["Mentor", "Player", "chat", "window"] <- 10 # (0,1,0,1)
multidim_arr["Player", "Player", "chat", "window"] <- 11 # (1,1,0,1)

multidim_arr["Mentor", "Mentor", "resource", "window"] <- 12 # (0,0,1,1)
multidim_arr["Player", "Mentor", "resource", "window"] <- 13 # (1,0,1,1)
multidim_arr["Mentor", "Player", "resource", "window"] <- 14 # (0,1,1,1)
multidim_arr["Player", "Player", "resource", "window"] <- 15 # (1,1,1,1)


# context_lookup <- as.matrix(mock_data[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(sender_cols, receiver_cols, mode_column)])
# 
# unit_1_context <- context_model$model$contexts[[2]];
# unit_1_context_lookup <- as.matrix(unit_1_context[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(sender_cols, receiver_cols, mode_column)]);
# unit_1_rows <- attr(unit_1_context, "tma.unit_rows")
# unit_1_to_last <- seq.int(last(unit_1_rows));
# res <- apply_windows_d(
#   multidim_arr,
#   attr(multidim_arr, "dim"),
#   attr(multidim_arr, "sender_inds") - 1,
#   attr(multidim_arr, "receiver_inds") - 1,
#   attr(multidim_arr, "mode_inds") - 1,
#   unit_1_context_lookup[unit_1_to_last,] - 1,
#   unit_1_rows - 1,
#   as.matrix(unit_1_context[unit_1_to_last, ..codes]),
#   unit_1_context[unit_1_to_last,][[time_column]]
# )

result_cpp <- accum_multidim_c(
  context_model = context_model, multidim_arr = multidim_arr, time_column = time_column, codes = codes
)

# etc. this seems to be working now 
# calculate_1d_index(c(1,0,1,0), as.vector(attr(multidim_arr, "dim"))) -- old version: this should be 5, but it fails and returns 10
# calculate_1d_index(c(1,0,1,0), as.vector(attr(multidim_arr, "dim"))) -- new version: this should be 5, and it succeeds and returns 5 

result_new <- accum_multidim(
  mock_data,
  multidim_arr,
  time_column,
  codes,
  context_model
)

# res <- apply_windows_d(
#   multidim_arr,
#   attr(multidim_arr, "dim"),
#   attr(multidim_arr, "sender_inds") - 1,
#   attr(multidim_arr, "receiver_inds") - 1,
#   attr(multidim_arr, "mode_inds") - 1,
#   unit_1_context_lookup[unit_1_to_last,] - 1,
#   unit_1_rows - 1,
#   as.matrix(unit_1_context[unit_1_to_last, ..codes]),
#   unit_1_context[unit_1_to_last,][[time_column]]
# )

# result_new$connection.counts[result_new$connection.counts$userID == "User1"]

# # result_new <- accum_multidim(
# #   mock_data,
# #   multidim_arr, 
# #   time_column,
# #   codes,
# #   context_model
# # )

# 
# # # dims <- dim(multidim_arr)
# # # res_as_array <- array(unlist(res), dim = dims);
# # # all(res_as_array == multidim_arr)
# 
# # result_cpp <- lapply(context_model$model$contexts, function(ctx) {
# #   # unit_1_context <- context_model$model$contexts$`User1::FirstHalf`;
# #   unit_1_context_lookup <- as.matrix(ctx[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(sender_cols, receiver_cols, mode_column)]);
# #   unit_1_rows <- attr(ctx, "tma.unit_rows")
# #   unit_1_to_last <- seq.int(last(unit_1_rows));
# # 
# #   res <- apply_windows_c(
# #     multidim_arr,
# #     attr(multidim_arr, "dim"),
# #     attr(multidim_arr, "sender_inds") - 1,
# #     attr(multidim_arr, "receiver_inds") - 1,
# #     attr(multidim_arr, "mode_inds") - 1,
# #     unit_1_context_lookup[unit_1_to_last,] - 1,
# #     unit_1_rows - 1,
# #     as.matrix(ctx[unit_1_to_last, ..codes]),
# #     ctx[unit_1_to_last,][[time_column]]
# #   )
# # 
# #   # dims <- dim(multidim_arr)
# #   # res_as_array <- array(unlist(res), dim = dims);
# #   # all(res_as_array == multidim_arr)
# # 
# #   res
# # })
*/