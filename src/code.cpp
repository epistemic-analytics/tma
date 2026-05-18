// [[Rcpp::depends(RcppArmadillo, libqe)]]

#include <RcppArmadillo.h>
#include <libqe/libqe.hpp>

//' @title n choose 2
//' @param n int
// [[Rcpp::export]]
int choose_two(int n) {
  return qe::choose_two(n);
}

arma::rowvec vector_to_summed_uppertri(arma::vec blank) {
  return qe::directed_to_upper_tri(blank);
}

// [[Rcpp::export]]
Rcpp::List accumulate_network(
  std::string unit,
  SEXP unit_rows_sexp,
  Rcpp::NumericVector code_cols,
  Rcpp::Function decay_function,
  int time_col = -1,
  bool ordered = true
) {
  Rcpp::DataFrame unit_rows_df = Rcpp::as<Rcpp::DataFrame>(Rcpp::wrap(unit_rows_sexp));

  // Build code matrix
  Rcpp::NumericMatrix codes_nm(unit_rows_df.nrow(), code_cols.size());
  for (int i = 0; i < code_cols.size(); i++) {
    Rcpp::NumericVector col = unit_rows_df[code_cols[i]];
    codes_nm(Rcpp::_, i) = col;
  }
  arma::mat codes_arma = Rcpp::as<arma::mat>(codes_nm);

  // Find indices of rows belonging to this unit
  Rcpp::CharacterVector qeunit_col = unit_rows_df["QEUNIT"];
  std::vector<int> unit_row_indices;
  for (int i = 0; i < qeunit_col.size(); i++) {
    if (qeunit_col[i] == unit) unit_row_indices.push_back(i);
  }

  Rcpp::Environment decay_fun_env = decay_function.environment();

  // Build decay callback: sets R env vars per iteration, then calls decay_function
  auto cpp_decay = [&](int unit_response_row, arma::uvec ground_indices) -> arma::vec {
    Rcpp::NumericVector ground_rows_r =
        Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(ground_indices));

    Rcpp::NumericVector ground_dist;
    if (time_col == -1) {
      ground_dist = Rcpp::seq(0, static_cast<int>(ground_indices.n_elem) - 1);
    } else {
      arma::uvec times_col = Rcpp::as<arma::uvec>(Rcpp::wrap(unit_rows_df[time_col]));
      arma::uvec g_times   = times_col.elem(ground_indices);
      arma::uword resp_t   = times_col[unit_response_row];
      arma::uvec resp_vec(g_times.n_elem, arma::fill::value(resp_t));
      ground_dist = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(resp_vec - g_times));
    }

    decay_fun_env["UNIT"]           = unit;
    decay_fun_env["RESPONSE_INDEX"] = unit_response_row + 1;
    decay_fun_env["ROWS"]           = ground_indices + 1;

    Rcpp::List ctx(unit_rows_df.ncol());
    for (int j = 0; j < unit_rows_df.ncol(); j++) {
      Rcpp::CharacterVector v = unit_rows_df(j);
      ctx[j] = v[ground_rows_r];
    }
    ctx.attr("names") = unit_rows_df.attr("names");
    decay_fun_env["FULL_CONTEXT"] = Rcpp::DataFrame(ctx);

    return Rcpp::as<arma::vec>(Rcpp::wrap(decay_function(ground_dist)));
  };

  qe::UnitNetworks result =
      qe::accumulate_unit_with_rows(codes_arma, unit_row_indices, cpp_decay, ordered);

  Rcpp::NumericMatrix row_networks_nm =
      Rcpp::as<Rcpp::NumericMatrix>(Rcpp::wrap(result.row_networks));
  Rcpp::NumericVector unit_row_indices_r =
      Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(unit_row_indices));
  Rcpp::Function subset("[.data.frame");
  row_networks_nm.attr("unit_rows") =
      subset(unit_rows_df, unit_row_indices_r + 1, R_MissingArg);
  Rcpp::CharacterVector classes = {"unit.rows", "list"};
  row_networks_nm.attr("class") = classes;

  return Rcpp::List::create(
    Rcpp::Named("networks")     = Rcpp::as<std::vector<double>>(Rcpp::wrap(result.networks)),
    Rcpp::Named("row_networks") = row_networks_nm
  );
}

//' fast accumulate networks
//' @param x TBD
//' @param code_cols TBD
//' @param decay_function TBD
//' @param time_col TBD
//' @param ordered TBD
//' 
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

// [[Rcpp::export]]
arma::mat calculate_adjacency_matrix(
  arma::rowvec ground, arma::rowvec response, double response_weight,
  bool ordered = true
) {
  return qe::calculate_adjacency_matrix(ground, response, response_weight, ordered);
}

// [[Rcpp::export]]
arma::rowvec adjacency_matrix_to_vector(arma::mat x, bool full = true) {
  return qe::adjacency_matrix_to_vector(x, full);
}

// [[Rcpp::export]]
int calculate_1d_index(const std::vector<int>& indices, const std::vector<int>& dims) {
  return qe::calculate_1d_index(indices, dims);
}

// [[Rcpp::export]]
Rcpp::List apply_tensor(
   Rcpp::NumericVector tensor,
   Rcpp::IntegerVector dims,
   std::vector<int> dims_sender,
   std::vector<int> dims_receiver,
   std::vector<int> dims_mode,
   Rcpp::NumericMatrix context_matrix,
   std::vector<int> unit_rows,
   arma::mat codes,
   Rcpp::NumericVector times,
   bool ordered = true
) {
  arma::imat ctx = arma::conv_to<arma::imat>::from(Rcpp::as<arma::mat>(context_matrix));
  qe::TensorNetworks r = qe::apply_tensor_unit(
    Rcpp::as<arma::vec>(tensor),
    std::vector<int>(dims.begin(), dims.end()),
    dims_sender, dims_receiver, dims_mode,
    ctx, unit_rows, codes,
    Rcpp::as<arma::vec>(times),
    ordered
  );
  return Rcpp::List::create(
    Rcpp::Named("row_connection_counts") = r.row_connection_counts,
    Rcpp::Named("connection_counts")     = r.connection_counts
  );
}

//' Apply windowing and weighting to context data for network accumulation (C++ backend)
//'
//' @param tensor NumericVector. Flat multi-dimensional context_tensor array.
//' @param dims IntegerVector. Dimensions of the original context_tensor array.
//' @param dims_sender std::vector<int>. Indices of sender dimensions.
//' @param dims_receiver std::vector<int>. Indices of receiver dimensions.
//' @param dims_mode std::vector<int>. Indices of mode dimensions.
//' @param context_matrix NumericMatrix. Context lookup matrix (rows = context lines, cols = factors).
//' @param unit_rows std::vector<int>. 0-based response-row indices for this unit.
//' @param codes arma::mat. Code matrix (nrow = context lines, ncol = number of codes).
//' @param times NumericVector. Timestamp per context row.
//' @param ordered bool. TRUE = directed; FALSE = undirected upper-triangle.
//' @return List with `row_connection_counts` and `connection_counts`.
//' @export
// [[Rcpp::export]]
Rcpp::List apply_tensor_old(
   Rcpp::NumericVector tensor,
   Rcpp::IntegerVector dims,
   std::vector<int> dims_sender,
   std::vector<int> dims_receiver,
   std::vector<int> dims_mode,
   Rcpp::NumericMatrix context_matrix,
   std::vector<int> unit_rows,
   arma::mat codes,
   Rcpp::NumericVector times,
   bool ordered = true
) {
  return apply_tensor(tensor, dims, dims_sender, dims_receiver, dims_mode,
                      context_matrix, unit_rows, codes, times, ordered);
}


// @title Indices representing an adjacnecey key
// @description Create a matrix of indices representing a co-occurrence
//              adjacency vector.  `len` represents the length of a side in a
//              square matrix.
// @param len Integer
// @param row Which row(s) to return, default to -1, returning both rows. 0
//            returns the top row, 1 will return the bottom row
//
// @return matrix with two rows
// [[Rcpp::export]]
arma::umat triIndices(int len, int row = -1) {
  return qe::tri_indices(len, row);
}


/*** R
#data(test_mockdata, package = "tma")
#mock_data <- test_mockdata;
#mock_data <- mock_data[mock_data$chatGroup == "PAM",]
#
## Initialize TMA units and contexts
#unit_cols <- c("userID", "condition")
#codes <- c("A", "B", "C")
#HOO_rules_model <- tma:::rules(
#  modality %in% "chat" & chatGroup %in% UNIT$chatGroup & condition %in% UNIT$condition, modality %in% "resource" & userID %in% UNIT$userID & condition %in% UNIT$condition
#)
#
#context_model <- tma:::contexts(
#  x = mock_data,
#  units = unit_cols,
#  hoo_rules = HOO_rules_model
#)
#
#time_column = "timeStamp"
#
## Get new TMA results 
#send_cols <- c()
#recv_cols <- c()
#mode_column = "modality"
#
#multidim_arr <- make_multidim_arr(mock_data, sender_cols = c(), receiver_cols = c(), mode_column, 0, 0)
#
#multidim_arr["chat", "weight"] <- 1
#multidim_arr["resource", "weight"] <- 2
#multidim_arr["chat", "window"] <- 360
#multidim_arr["resource", "window"] <- 180
#
#ctx <- context_model$model$contexts[[3]];
#unit_context_lookup <- as.matrix(ctx[, lapply(.SD, function(x) as.numeric(as.factor(x))), .SDcols = c(send_cols, recv_cols, mode_column)]);
#unit_rows <- attr(ctx, "tma.unit_rows")
#unit_to_last <- seq.int(last(unit_rows));
#res <- apply_tensor(
#  multidim_arr,
#  attr(multidim_arr, "dim"),
#  attr(multidim_arr, "sender_inds") - 1,
#  attr(multidim_arr, "receiver_inds") - 1,
#  attr(multidim_arr, "mode_inds") - 1,
#  unit_context_lookup[unit_to_last,, drop = FALSE] - 1,
#  unit_rows - 1,
#  as.matrix(ctx[unit_to_last, ..codes]),
#  ctx[unit_to_last,][[time_column]]
#);
#print(res$connection_counts)
# user2_old <- as.numeric(unname(as.vector(result_old$connection.counts[1,4:length(colnames(result_old$connection.counts))])))
# # user2_new <- as.numeric(unname(as.vector(result_new$connection.counts[2,4:length(colnames(result_new$connection.counts))])))
# user2_new <- res$connection_counts[1,]
# testthat::expect_equal(user2_old, user2_new)
# 
# res_2 <- apply_windows_d(
#   multidim_arr,
#   attr(multidim_arr, "dim"),
#   attr(multidim_arr, "sender_inds") - 1,
#   attr(multidim_arr, "receiver_inds") - 1,
#   attr(multidim_arr, "mode_inds") - 1,
#   unit_context_lookup[unit_to_last,, drop = FALSE] - 1,
#   unit_rows - 1,
#   as.matrix(ctx[unit_to_last, ..codes]),
#   ctx[unit_to_last,][[time_column]],
#   ordered = FALSE
# );
# print(res$connection_counts)
# print(res_2$connection_counts)
#
*/