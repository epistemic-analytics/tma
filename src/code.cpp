// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

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
  NumericVector code_cols,
  Function decay_function,
  int time_col = -1,
  bool ordered = true
) {
  // Rcpp::Rcout << "1" << std::endl;
  arma::vec blank(code_cols.size() * code_cols.size());
  Rcpp::DataFrame unit_rows = Rcpp::as<Rcpp::DataFrame>(wrap(unit_rows_sexp));
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
    Rcpp::NumericMatrix::Column codes_col = codes(_, i);
    codes_col = unit_code_col;
  }

  // Environment decay_fun_env(decay_function);
  Environment decay_fun_env = decay_function.environment();
  
  // Rcpp::Rcout << "3" << std::endl;
  for(int i = 0; i < qeunit_row_indices.size(); i++) {
    int unit_response_row = qeunit_row_indices[i];
    
    arma::uvec ground_rows = arma::regspace<uvec>(0, 1, unit_response_row);
    Rcpp::NumericVector ground_rows2 = Rcpp::as<Rcpp::NumericVector>(wrap(ground_rows));
    NumericVector ground_dist_from_response(ground_rows.size());
    if (time_col == -1) {
      ground_dist_from_response = seq(0, ground_rows.size()-1);
      std::reverse(ground_dist_from_response.begin(), ground_dist_from_response.end());
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
    
    arma::vec decay_effect = as<arma::vec>(wrap(decay_function(ground_dist_from_response)));

    NumericMatrix ground_codes_raw = codes( Range( ground_rows[0], ground_rows[ground_rows.size() - 1]  ), _);
    
    arma::mat ground_codes_mat = as<arma::mat>(ground_codes_raw);
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

  NumericMatrix individual_networks_nm = Rcpp::as<Rcpp::NumericMatrix>(Rcpp::wrap(individual_networks));
  
  Function subset("[.data.frame");
  individual_networks_nm.attr("unit_rows") = subset(unit_rows, qeunit_row_indices + 1, R_MissingArg);
  CharacterVector classes = {"unit.rows","list"};
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
    Rcpp::Named("networks") = as<std::vector<double>>(wrap(network_vector)),
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
  List x,
  NumericVector code_cols,
  Function decay_function,
  int time_col = -1,
  bool ordered = true
) {
  List model = x["model"];
  List contexts = model["contexts"];
  CharacterVector units = model["unit.labels"];
  
  // Rcpp::Rcout << "N codes: " << code_cols.size() << std::endl;
  int ncol = code_cols.size() * code_cols.size();
  if(ordered == false) {
    ncol = choose_two(code_cols.size());
    // Rcpp::Rcout << "Ncol: " << ncol << std::endl;
  }
  NumericMatrix blank(units.size(), ncol);
  Rcpp::List unit_networks_by_row = Rcpp::List::create();
  
  for(int i = 0; i < units.size(); i++) {
    NumericMatrix::Row unit_row = blank.row(i);
    std::string unit_name = as<std::string>(wrap(units[i]));
    
    Rcpp::List accumulated = accumulate_network(unit_name, contexts[unit_name], code_cols, decay_function, time_col, ordered);
    NumericVector unit_network_conv = as<NumericVector>(wrap(accumulated["networks"]));
    NumericMatrix row_networks = accumulated["row_networks"];
    
    unit_networks_by_row[unit_name] = row_networks;
    unit_row = unit_network_conv;
  }
  
  return Rcpp::List::create(
    Rcpp::Named("networks") = blank,
    Rcpp::Named("networks_by_row") = unit_networks_by_row
  );
}

#include <iostream>
#include <vector>

template <typename T>
T* allocateArray(const std::vector<int>& sizes, int dimension = 0) {
  if (dimension == sizes.size() - 1) {
    return new T[sizes[dimension]];
  }
  else {
    T** array = new T*[sizes[dimension]];
    for (int i = 0; i < sizes[dimension]; ++i) {
      array[i] = allocateArray<T>(sizes, dimension + 1);
    }
    return reinterpret_cast<T*>(array);
  }
}

template <typename T>
T* allocateAndInitializeArray(
  const std::vector<int>& sizes, 
  const std::vector<T>& values, 
  int& index, int dimension = 0
) {
  if (dimension == sizes.size() - 1) {
    T* array = new T[sizes[dimension]];
    for (int i = 0; i < sizes[dimension]; ++i) {
      array[i] = values[index++];
    }
    return array;
  }
  else {
    T** array = new T*[sizes[dimension]];
    for (int i = 0; i < sizes[dimension]; ++i) {
      array[i] = allocateAndInitializeArray<T>(sizes, values, index, dimension + 1);
    }
    return reinterpret_cast<T*>(array);
  }
}

template <typename T>
T* allocateAndInitializeArrayRev(
    const std::vector<int>& sizes, 
    const std::vector<T>& values, 
    int& index, int dimension = 0, int indent = 0
) {
  // Rcpp::Rcout << dimension << std::endl;
  // if (dimension == sizes.size() - 1) {
  if (dimension == 0) {
    T* array = new T[sizes[dimension]];
    for (int i = 0; i < sizes[dimension]; ++i) {
      int add_val = values[index++];
      // std::cout << std::string(indent, ' ') << "Dimension " << dimension << " -> " << "Adding value: " << add_val << std::endl;
      array[i] = add_val;
    }
    return array;
  }
  else {
    T** array = new T*[sizes[dimension]];
    for (int i = 0; i < sizes[dimension]; ++i) {
      // std::cout << "Dimension: " << i << std::endl;
      // std::cout << std::string(indent, ' ') << "Dimension " << dimension << " - " << i << """:" << std::endl;
      array[i] = allocateAndInitializeArrayRev<T>(sizes, values, index, dimension - 1, ++indent);
    }
    return reinterpret_cast<T*>(array);
  }
}

template <typename T>
void deallocateArray(T* array, const std::vector<int>& sizes, int dimension = 0) {
  if (dimension == sizes.size() - 1) {
    delete[] array;
  } else {
    for (int i = 0; i < sizes[dimension]; ++i) {
      deallocateArray(reinterpret_cast<T**>(array)[i], sizes, dimension + 1);
    }
    delete[] reinterpret_cast<T**>(array);
  }
}

std::vector<std::vector<int>> matrix_to_2d_vector(NumericMatrix mat) {
  int n = mat.nrow();
  std::vector<std::vector<int>> vec(n);
  
  for (int i = 0; i < n; ++i) {
    NumericVector row = mat(i, _); // Extract the i-th row
    vec[i] = Rcpp::as<std::vector<int>>(row); // Convert the row to std::vector<int>
  }
  return vec;
}
std::vector<std::vector<int>> armamatrix_to_2d_vector(arma::mat mat) {
  int n = mat.n_rows;
  std::vector<std::vector<int>> vec(n);
  
  for (int i = 0; i < n; ++i) {
    arma::rowvec row = mat.row(i); // Extract the i-th row
    // vec[i] = Rcpp::as<std::vector<int>>(row); // Convert the row to std::vector<int>
    
    std::vector<int> std_vec(row.begin(), row.end());
    vec[i] = std_vec;
  }
  return vec;
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

template <typename T>
void printArray(T* array, const std::vector<int>& sizes, int dimension = 0, int indent = 0) {
  if (dimension == sizes.size() - 1) {
    for (int i = 0; i < sizes[dimension]; ++i) {
      std::cout << std::string(indent, ' ') << array[i] << " ";
    }
    std::cout << std::endl;
  } 
  else {
    for (int i = 0; i < sizes[dimension]; ++i) {
      std::cout << std::string(indent, ' ') << "Dimension " << dimension << ", Index " << i << ":" << std::endl;
      printArray(reinterpret_cast<T**>(array)[i], sizes, dimension + 1, indent + 2);
    }
  }
}

template <typename T>
SEXP createListFromMultiDimArray(T* array, const std::vector<int>& sizes, int dimension = 0) {
  if (dimension == sizes.size() - 1) {
    NumericVector vec(sizes[dimension]);
    for (int i = 0; i < sizes[dimension]; ++i) {
      vec[i] = array[i];
    }
    return vec;
  } else {
    List list(sizes[dimension]);
    for (int i = 0; i < sizes[dimension]; ++i) {
      list[i] = createListFromMultiDimArray(reinterpret_cast<T**>(array)[i], sizes, dimension + 1);
    }
    return list;
  }
}

template <typename T>
T getElement(T* array, const std::vector<int>& indices, int dimension = 0) {
  if (dimension == indices.size() - 1) {
    return array[indices[dimension]];
  } else {
    T** arrayNext = reinterpret_cast<T**>(array);
    return getElement(arrayNext[indices[dimension]], indices, dimension + 1);
  }
}

NumericMatrix subsetRowsByIndices(const NumericMatrix& mat, const std::vector<int>& indices) {
  int nrows = indices.size();
  int ncols = mat.ncol();
  NumericMatrix result(nrows, ncols);
  
  for (int i = 0; i < nrows; ++i) {
    int rowIndex = indices[i];
    if (rowIndex >= 0 && rowIndex < mat.nrow()) {
      for (int j = 0; j < ncols; ++j) {
        result(i, j) = mat(rowIndex, j);
      }
    } else {
      Rcpp::stop("Index out of bounds");
    }
  }
  
  return result;
}

// Function to insert elements dynamically
void insert_element(arma::uvec& vec, double value) {
  // Add a new row at the end
  vec.insert_rows(vec.n_rows, 1);
  
  // Assign the value to the new element
  vec(vec.n_rows - 1) = value;
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

//' @export
// [[Rcpp::export]]
Rcpp::List apply_windows_c(
  std::vector<int> multidim_as_vector, 
  std::vector<int> dims,
  std::vector<int> dims_sender,
  std::vector<int> dims_receiver,
  std::vector<int> dims_mode,
  NumericMatrix context_matrix,
  std::vector<int> unit_rows,
  arma::mat codes,
  NumericVector times
) {
  int code_cnt = codes.n_cols;
  
  std::vector<std::vector<int>> context_lookup = matrix_to_2d_vector(context_matrix);
  // std::vector<std::vector<int>> codes_matrix = armamatrix_to_2d_vector(codes);
  
  int index = 0;
  int dim_start = dims.size() - 1;
  int* multidim_as_array2 = allocateAndInitializeArrayRev<int>(dims, multidim_as_vector, index, dim_start);
  
  // printArray(multidim_as_array2, dims, 0);
  // print_2d_vector(context_lookup);
  // print_2d_vector(codes_matrix);
  // std::cout << "Codes matrix: " << std::endl << codes << std::endl;
 
  arma::mat g_w_vec(code_cnt, code_cnt, arma::fill::zeros);
  arma::mat unit_row_connection_counts(unit_rows.size(), code_cnt * code_cnt);
  arma::uvec upper_indices = trimatu_ind( size(g_w_vec), 1 );
  // std::cout << "UI: " << upper_indices << std::endl;
  
  for(int i = 0; i < unit_rows.size(); ++i) {
    int unit_row_i = unit_rows[i];
    // std::cout << "Unit row: " << unit_row_i << std::endl;

    std::vector<int> response_context_lookup = context_lookup[unit_row_i];
    arma::rowvec row_vec = codes.row(unit_row_i);
    double response_time = times[unit_row_i];

    // std::cout << "Response context: "; print_vector(response_context_lookup);
    // std::cout << "Response codes: " << std::endl << row_vec << std::endl;
    // std::cout << "Response time: " << response_time << std::endl;
    
    // std::cout << "Response: " << std::endl;
    // print_vector(response_context_lookup);
    // std::cout << std::endl;
    response_context_lookup.push_back(1);
    
    std::reverse(response_context_lookup.begin(), response_context_lookup.end());
    int response_win = getElement(multidim_as_array2, response_context_lookup);
    response_context_lookup[0] = 0;
    int response_weight = getElement(multidim_as_array2, response_context_lookup);

    arma::uvec ground_rows_to_include;
    // ground_rows_to_include.set_size(unit_row_i);
    // std::vector<int> ground_rows_to_include;
    // ground_rows_to_include.reserve(unit_row_i);

    arma::colvec ground_row_weights;
    // ground_row_weights.set_size(unit_row_i);
    
    if(unit_row_i > 0) {
      std::vector<std::vector<int>> context_ground = std::vector<std::vector<int>>(context_lookup.begin(), context_lookup.begin() + unit_row_i);

      // std::cout << "Ground: " << std::endl;
      // print_2d_vector(context_ground);
      // std::cout << std::endl;

      for (int ground_row = unit_row_i - 1; ground_row >= 0; ground_row --) {
        std::vector<int> row = context_ground[ground_row];
        double ground_row_time = times[ground_row];

        // Adjust array lookup to account for response row values
        for (const auto& dim : dims_receiver) {
          row[dim] = response_context_lookup[dim];
        }

        row.push_back(1);
        
        std::reverse(row.begin(), row.end());
        double row_win = getElement(multidim_as_array2, row);
        double ground_row_time_adj = ground_row_time + row_win;
        double diff = ground_row_time_adj - response_time;
        
        // std::cout << " -> g time: " << ground_row_time << std::endl;
        // std::cout << " -> row_win: " << row_win << std::endl;
        // std::cout << " -> adjusted: " << ground_row_time_adj << std::endl;
        // std::cout << " -> r time: " << response_time << std::endl;
        // std::cout << " -> diff: " << diff << std::endl;
        
        if(diff >= 0) {
          // std::cout << "Found ground row: " << ground_row << std::endl;
          insert_element(ground_rows_to_include, ground_row);
          // ground_rows_to_include.push_back(ground_row);
          
          row[0] = 0;
          // std::cout << "Weight r lookup: "; print_vector(row);
          int row_wgt = getElement(multidim_as_array2, row);
          // std::cout << "Weight: " << row_wgt << std::endl;
          
          ground_row_weights.insert_rows(ground_row_weights.n_rows, 1);
          ground_row_weights(ground_row_weights.n_rows - 1) = row_wgt;
        }
      }
      
      if(ground_rows_to_include.size() > 0) {
        arma::mat ground_codes = codes.rows(ground_rows_to_include);
        arma::mat ground_codes_by_weight = ground_codes.each_col() % ground_row_weights;
        arma::rowvec g_ws_vec = arma::sum(ground_codes_by_weight);
        arma::rowvec r_vec = codes.row(unit_row_i);
        
        // std::cout << "g_ws_vec: " << std::endl << ground_codes << std::endl;
        // std::cout << "r_vec: " << std::endl << r_vec << std::endl;
        
        arma::mat counting_matrix = response_weight *(row_vec.t() * row_vec);
        // std::cout << "CM 1: " << std::endl << counting_matrix << std::endl;
        
        arma::mat counting_matrix3 = row_vec.t() * g_ws_vec;
        arma::mat counting_matrix2 = counting_matrix3.t();
        // std::cout << "CM 2: " << std::endl << counting_matrix2 << std::endl;
        // std::cout << "CM 3: " << std::endl << counting_matrix3 << std::endl;
        
        // Directed
        // (g_ws_vec %*% t(r_vec)) + 0.5 * w_rr * f_diag(r_vec %*% t(r_vec))
        
        // Only generating for standard ENA currently
        arma::mat resp = calculate_adjacency_matrix(g_ws_vec, row_vec, response_weight);
        g_w_vec = g_w_vec + resp;
        
        unit_row_connection_counts.row(i) = adjacency_matrix_to_vector(resp);
      }
      else {
        // std::cout << "-- No Ground Rows Passed --" << std::endl;
      }
    }

    // No ground available
    else {
      // std::cout << "-- No Ground --" << std::endl;
    }
  }
  
  deallocateArray(multidim_as_array2, dims);

  arma::rowvec unit_connection_counts = adjacency_matrix_to_vector(g_w_vec);
  
  Rcpp::List return_list = Rcpp::List::create(
    Rcpp::Named("row_connection_counts") = unit_row_connection_counts,
    Rcpp::Named("connection_counts") = unit_connection_counts
  );
  
  return(return_list);
}

/*** R
data(test_mockdata, package = "tma")
mock_data <- test_mockdata;
mock_data <- mock_data[mock_data$chatGroup == "PAM",]
sender_cols <- c("role")
receiver_cols <- c("role")
mode_column <- "modality"

multidim_arr <- make_multidim_arr(mock_data, sender_cols = sender_cols, receiver_cols = receiver_cols, mode_column = mode_column, 20, 1)
# multidim_arr["Mentor", "L1", "chat", "weight"] <- 1
# multidim_arr["Mentor", "L1", "resource", "weight"] <- 1
# multidim_arr["Mentor", "L2", "chat", "weight"] <- 1
# multidim_arr["Mentor", "L2", "resource", "weight"] <- 1
# multidim_arr["Player", "L1", "chat", "weight"] <- 1
# multidim_arr["Player", "L1", "resource", "weight"] <- 1
# multidim_arr["Player", "L2", "chat", "weight"] <- 1
# multidim_arr["Player", "L2", "resource", "weight"] <- 1

# multidim_arr["Mentor", "L1", "chat", "window"] <- 800
# multidim_arr["Mentor", "L1", "resource", "window"] <- 500
# multidim_arr["Mentor", "L2", "chat", "window"] <- 20
# multidim_arr["Mentor", "L2", "resource", "window"] <- 310
# multidim_arr["Player", "L1", "chat", "window"] <- 320
# multidim_arr["Player", "L1", "resource", "window"] <- 200
# multidim_arr["Player", "L2", "chat", "window"] <- 150
# multidim_arr["Player", "L2", "resource", "window"] <- 360

# dims <- dim(multidim_arr)
# multidim_arr <- seq_along(multidim_arr)
# dim(multidim_arr) <- dims

unit_cols <- c("userID", "condition")
codes <- c("A", "B", "C")
HOO_rules_model <- tma:::rules(
  modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
)
context_model <- tma:::contexts(
  x = mock_data,
  units = unit_cols,
  hoo_rules = HOO_rules_model
)
# time_column = "timeStamp"
time_column = tma:::ATTR_NAMES$CONTEXT_COL_ID
context_lookup <- as.matrix(mock_data[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(sender_cols, receiver_cols, mode_column)])

unit_1_context <- context_model$model$contexts$`User1::FirstHalf`;
unit_1_context_lookup <- as.matrix(unit_1_context[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(sender_cols, receiver_cols, mode_column)]);
unit_1_rows <- attr(context_model$model$contexts$`User1::FirstHalf`, "tma.unit_rows")
unit_1_to_last <- seq.int(last(unit_1_rows));

res <- apply_windows_c(
  multidim_arr,
  attr(multidim_arr, "dim"),
  attr(multidim_arr, "sender_inds") - 1,
  attr(multidim_arr, "receiver_inds") - 1,
  attr(multidim_arr, "mode_inds") - 1,
  unit_1_context_lookup[unit_1_to_last,] - 1,
  unit_1_rows - 1,
  as.matrix(unit_1_context[unit_1_to_last, ..codes]),
  unit_1_context[unit_1_to_last,][[time_column]]
)

result_new <- accum_multidim(
  mock_data,
  multidim_arr, 
  time_column,
  codes,
  context_model
)

# dims <- dim(multidim_arr)
# res_as_array <- array(unlist(res), dim = dims);
# all(res_as_array == multidim_arr)


result_cpp <- lapply(context_model$model$contexts, function(ctx) {
  # unit_1_context <- context_model$model$contexts$`User1::FirstHalf`;
  unit_1_context_lookup <- as.matrix(ctx[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(sender_cols, receiver_cols, mode_column)]);
  unit_1_rows <- attr(ctx, "tma.unit_rows")
  unit_1_to_last <- seq.int(last(unit_1_rows));

  res <- apply_windows_c(
    multidim_arr,
    attr(multidim_arr, "dim"),
    attr(multidim_arr, "sender_inds") - 1,
    attr(multidim_arr, "receiver_inds") - 1,
    attr(multidim_arr, "mode_inds") - 1,
    unit_1_context_lookup[unit_1_to_last,] - 1,
    unit_1_rows - 1,
    as.matrix(ctx[unit_1_to_last, ..codes]),
    ctx[unit_1_to_last,][[time_column]]
  )

  # dims <- dim(multidim_arr)
  # res_as_array <- array(unlist(res), dim = dims);
  # all(res_as_array == multidim_arr)

  res
})
*/