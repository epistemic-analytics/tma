// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

// std::vector<double> accumulate_network(
// [[Rcpp::export]]
Rcpp::List accumulate_network(
  std::string unit,
  SEXP unit_rows_sexp, 
  NumericVector code_cols,
  Function decay_function,
  int time_col = -1
) {
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
  // Rcpp::Rcout << "qeunit_row_indices: " << std::endl << qeunit_row_indices << std::endl;

  // Environment decay_fun_env(decay_function);
  Environment decay_fun_env = decay_function.environment();
  // Rcpp::Rcout << "Got the environment." << std::endl;
  decay_fun_env["UNIT"] = unit;
  
  // Rcpp::Rcout << "Set in the environment." << std::endl;
  for(int i = 0; i < qeunit_row_indices.size(); i++) {
    int unit_response_row = qeunit_row_indices[i];
    
    arma::uvec ground_rows = arma::regspace<uvec>(0, 1, unit_response_row);
    // if((unit_response_row - 1) >= 0) {
    //   NumericVector first_row_to_this_one = as<NumericVector>(wrap(seq_len(unit_response_row - 1)));
    //   
    //   for(int j = 0; j < (unit_response_row); j++) {
    //     ground_rows.push_back(j);
    //   }
    // }
    // ground_rows.push_back(unit_response_row);
    // Rcpp::Rcout << "Grd rows: " << (ground_rows) << std::endl;
    // Rcpp::Rcout << "Resp row: " << (unit_response_row) << std::endl;
    
    NumericVector ground_dist_from_response(ground_rows.size());
    if (time_col == -1) {
      ground_dist_from_response = seq(0, ground_rows.size()-1);
      std::reverse(ground_dist_from_response.begin(), ground_dist_from_response.end());
    }
    else {
      arma::uvec times = Rcpp::as<arma::uvec>(wrap(unit_rows[time_col]));
      // Rcpp::Rcout << "Times: " << std::endl << times << std::endl;
      arma::uvec ground_times = times.elem(ground_rows);
      // Rcpp::Rcout << "Grd Times: " << (ground_times) << std::endl;
      arma::uword response_time = ground_times[ground_times.size() - 1];
      arma::uvec response_times(ground_times.n_elem, arma::fill::value(response_time));
      // Rcpp::Rcout << "Resp Time: " << response_time << std::endl;
      
      ground_dist_from_response = response_times - ground_times;
      // ground_dist_from_response.fill(1);
      // Rcpp::Rcout << "Time to response: " << (ground_dist_from_response) << std::endl;
    }
    arma::vec decay_effect = as<arma::vec>(wrap(decay_function(ground_dist_from_response)));

    NumericMatrix ground_codes_raw = codes( Range( ground_rows[0], ground_rows[ground_rows.size() - 1]  ), _);
    // Rcpp::Rcout << "ground_codes_raw: " << std::endl << ground_codes_raw << std::endl;
    
    arma::mat ground_codes_mat = as<arma::mat>(ground_codes_raw);

    arma::mat codes_decayed(ground_codes_mat.n_rows, ground_codes_mat.n_cols);
    for(int j = 0; j < codes_decayed.n_rows; j ++) {
      codes_decayed.row(j) = ground_codes_mat.row(j) * decay_effect[j];
    }

    arma::rowvec response_row = codes_decayed.tail_rows(1);
    arma::rowvec ground_summarized = arma::sum(codes_decayed);
    // Rcpp::Rcout << "ground_summarized: " << std::endl << ground_summarized << std::endl;
    
    arma::rowvec ground_summarized_no_resp = ground_summarized - response_row;
    arma::mat connection_matrix = ground_summarized_no_resp.t() * response_row;
    // Rcpp::Rcout << "connection_matrix: " << std::endl << connection_matrix << std::endl;
    
    arma::mat self_connection_matrix = response_row.t() * response_row;
    self_connection_matrix.diag() *= 0;
    self_connection_matrix = self_connection_matrix * 0.5;
    // Rcpp::Rcout << "self_connection_matrix: " << std::endl << self_connection_matrix << std::endl;
    
    arma::mat full_connection_matrix = connection_matrix + self_connection_matrix;
    arma::vec full_connection_vec = arma::vectorise(full_connection_matrix);
    // Rcpp::Rcout << "full_connection_vec: " << std::endl << full_connection_vec << std::endl;
    
    individual_networks.row(i) = full_connection_vec.t();
    blank = blank + full_connection_vec;
  }

  NumericMatrix individual_networks_nm = Rcpp::as<Rcpp::NumericMatrix>(Rcpp::wrap(individual_networks));
  
  Function subset("[.data.frame");
  individual_networks_nm.attr("unit_rows") = subset(unit_rows, qeunit_row_indices + 1, R_MissingArg);
  CharacterVector classes = {"unit.rows","list"};
  individual_networks_nm.attr("class") = classes;

  return Rcpp::List::create(
    Rcpp::Named("networks") = as<std::vector<double>>(wrap(blank.t())),
    Rcpp::Named("row_networks") = individual_networks_nm
  );
}

//' fast accumulate networks
//' @param x TBD
//' @param code_cols TBD
//' @param decay_function TBD
//' @param time_col TBD
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List accumulate_networks(
  List x,
  NumericVector code_cols,
  Function decay_function,
  int time_col = -1
) {
  List model = x["model"];
  List contexts = model["contexts"];
  CharacterVector units = model["unit.labels"];
  
  NumericMatrix blank(units.size(), code_cols.size() * code_cols.size());
  Rcpp::List unit_networks_by_row = Rcpp::List::create();
  
  for(int i = 0; i < units.size(); i++) {
    NumericMatrix::Row unit_row = blank.row(i);
    std::string unit_name = as<std::string>(wrap(units[i]));
    // std::vector<double> unit_network = accumulate_network(unit_name, contexts[unit_name], code_cols, decay_function, time_col);
    // NumericVector unit_network_conv = as<NumericVector>(wrap(unit_network));
    // unit_row = unit_network_conv;
    
    Rcpp::List accumulated = accumulate_network(unit_name, contexts[unit_name], code_cols, decay_function, time_col);
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