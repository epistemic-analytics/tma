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
  decay_fun_env["UNIT"] = unit;
  
  // Rcpp::Rcout << "3" << std::endl;
  for(int i = 0; i < qeunit_row_indices.size(); i++) {
    int unit_response_row = qeunit_row_indices[i];
    
    arma::uvec ground_rows = arma::regspace<uvec>(0, 1, unit_response_row);
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

/*** R
test_smalldata = data.table::fread(system.file(package = "tma", 'extdata/test_data_3.csv'), stringsAsFactors = FALSE)
test_smalldata$Timestamp2 = as.numeric(as.POSIXct(test_smalldata$Timestamp));
test_smalldata[12,2] <- "User2";
hoo_rules <- conversation_rules(
  (Condition == UNIT$Condition & GroupName == UNIT$GroupName & Context == "Public"),
  (Condition == UNIT$Condition & GroupName == UNIT$GroupName & UserName == UNIT$UserName)
);  
units_by <- c("GroupName", "UserName");
code_cols <- which(colnames(test_smalldata) %in%  c("A","B","C"));
network_contexts <-  contexts(test_smalldata, units_by = units_by, hoo_rules = hoo_rules);
# networks2 <- accumulate_contexts(network_contexts, codes = code_cols, return.dena.set = TRUE, return.ena.set = FALSE);

# network2 <- accumulate_network(
#   "Group_1::User3", 
#   network_contexts$model$contexts$`Group_1::User3`, 
#   (code_cols - 1), 
#   decay_function = function(x) {
#     (x < 4) * 1 
#   }
# );
network2 <- accumulate_network("Group_1::User3", network_contexts$model$contexts$`Group_1::User3`, (code_cols - 1), decay_function = decay(simple_window, window_size = 4), time_col = 8);
network22 <- accumulate_network(
  "Group_1::User3", network_contexts$model$contexts$`Group_1::User3`, (code_cols - 1), 
  decay_function = function(x) {
    print(x);
    sqrt(x)
  },
  time_col = 8
);


all_networks <- accumulate_networks(x = network_contexts, code_cols = (code_cols - 1), decay_function = decay(simple_window, window_size = 4), time_col = 8)

y <- accumulate(
  x = network_contexts, 
  code_cols = code_cols,
  decay_function = decay(simple_window, window_size = 4)
)


# microbenchmark::microbenchmark(
#   old = accumulate_contexts(network_contexts, codes = code_cols, return.dena.set = TRUE, return.ena.set = FALSE),
#   cpp = accumulate_networks(network_contexts, (code_cols - 1), decay_function = decay(simple_window, window_size = 4))
# )
*/