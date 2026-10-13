// [[Rcpp::depends(RcppArmadillo)]]
// Rcpp wrappers for libtma, the accumulation layer of the libqe C++
// libraries.  The headers live in inst/include/libtma/ (moved from libqe in
// its phase 4b split); they build on libqe's generic numerics, which come
// from LinkingTo: libqe.  The wrappers moved from libqe's libqe_rcpp.cpp with
// the same names, bodies and docs, and stay exported: tma is now where R code
// (rENA, ona) gets accumulation from.
#include <RcppArmadillo.h>
#include <libtma/accumulation.hpp>

using namespace Rcpp;

// =============================================================================
// Accumulation
// =============================================================================

//' Core connection matrix for one ground+response pair
//' @param ground          Numeric row vector of ground (context) code values
//' @param response        Numeric row vector of response code values
//' @param response_weight Scalar weight applied to the response self-connection
//' @param ordered         TRUE = directed; FALSE = undirected
//' @export
// [[Rcpp::export]]
arma::mat connection_matrix(arma::rowvec ground, arma::rowvec response,
                                         double response_weight = 1.0,
                                         bool ordered = true) {
    return qe::connection_matrix(ground, response, response_weight, ordered);
}

//' Traditional stanza-window accumulation (rENA model)
//'
//' For each row k in a single conversation's code matrix, accumulates
//' connections over a back/forward window.
//'
//' \strong{Undirected} (\code{ordered = FALSE}, default): upper-triangle
//' co-occurrences with back/forward-reference corrections.
//' Returns \code{n_rows × choose_two(n_codes)}.
//'
//' \strong{Directed} (\code{ordered = TRUE}): focal row k as response,
//' sum of prior window rows as ground, using the directed connection formula.
//' \code{window_forward} is ignored. Returns \code{n_rows × n_codes²}.
//'
//' @param codes          Numeric matrix (rows = lines, cols = codes) for ONE conversation
//' @param window_back    Number of prior lines in window (default 1); use .Machine$integer.max for Inf
//' @param window_forward Number of subsequent lines (default 0; ignored when ordered = TRUE)
//' @param binary         If TRUE, binarise non-zero connection counts
//' @param ordered        If TRUE, return directed n² output; FALSE (default) returns upper-tri
//' @return Undirected: (n_rows × choose_two(n_codes)); directed: (n_rows × n_codes²)
//' @export
// [[Rcpp::export]]
arma::mat accumulate_stanza(arma::mat codes,
                            int window_back    = 1,
                            int window_forward = 0,
                            bool binary        = true,
                            bool ordered       = false) {
    return qe::accumulate_stanza(codes, window_back, window_forward, binary, ordered);
}

//' Compute a column-major linear index into a multi-dimensional array
//'
//' Equivalent to tma's flat_index(). Throws if lengths of `indices`
//' and `dims` differ.
//'
//' @param indices 0-based integer vector of per-dimension indices
//' @param dims    Integer vector of array dimensions
//' @return Scalar integer linear index
//' @export
// [[Rcpp::export]]
int flat_index(std::vector<int> indices, std::vector<int> dims) {
    return qe::flat_index(indices, dims);
}

//' Per-row upper-triangle co-occurrence matrix
//'
//' For each row in \code{codes}, computes the pairwise code products
//' (upper-triangle) and optionally binarizes. Output has
//' \code{choose(n_codes, 2)} columns.
//'
//' @param codes  Numeric matrix (rows = observations, cols = codes)
//' @param binary If TRUE, binarise non-zero co-occurrences (default TRUE)
//' @return Numeric matrix (n_rows x choose_two(n_codes))
//' @export
// [[Rcpp::export]]
arma::mat row_connections(arma::mat codes, bool binary = true) {
    return qe::row_connections(codes, binary);
}

//' Rolling backward window sum of a code matrix
//'
//' For each row k, sums the raw code values over rows
//' \code{[max(0, k - window_size + 1), k]}. Returns a matrix of the same
//' shape as \code{codes}. \code{window_size <= 0} is treated as 1
//' (current row only).
//'
//' @param codes       Numeric matrix (rows = observations, cols = codes)
//' @param window_size Number of rows to look back (default 1)
//' @return Numeric matrix (same dimensions as \code{codes})
//' @export
// [[Rcpp::export]]
arma::mat rolling_window_sum(arma::mat codes, int window_size = 1) {
    return qe::rolling_window_sum(codes, window_size);
}

//' Ground/response accumulation for one unit (tma model)
//'
//' @param codes      Numeric matrix for the full context (n_rows x n_codes)
//' @param unit_rows  0-based integer vector of rows belonging to this unit
//' @param decay_fn   R function mapping a numeric distance vector to weights
//' @param ordered    TRUE = directed; FALSE = undirected (upper-tri)
//' @return Numeric vector of connection counts
//' @export
// [[Rcpp::export]]
arma::rowvec accumulate_unit(arma::mat codes, std::vector<int> unit_rows,
                                  Function decay_fn, bool ordered = false) {
    auto cpp_decay = [&](arma::vec distances) -> arma::vec {
        NumericVector d = wrap(distances);
        NumericVector w = decay_fn(d);
        return as<arma::vec>(w);
    };
    return qe::accumulate_unit(codes, unit_rows, cpp_decay, ordered);
}

//' Ground/response accumulation for one unit — returns unit vector and per-row matrix
//'
//' Equivalent to tma's accumulate_network() but without R-env-var setup.
//' The decay function receives a distance vector (response=0, older rows
//' have larger values) and returns a weight vector of the same length.
//'
//' @param codes      Numeric matrix (n_rows x n_codes)
//' @param unit_rows  0-based integer vector of rows belonging to this unit
//' @param decay_fn   R function(distances) -> weights
//' @param ordered    TRUE = directed (full p^2); FALSE = undirected (upper-tri)
//' @return List with `networks` (vector) and `row_networks` (matrix)
//' @export
// [[Rcpp::export]]
List accumulate_unit_with_rows(arma::mat codes, std::vector<int> unit_rows,
                                   Function decay_fn, bool ordered = false) {
    auto cpp_decay = [&](int unit_row, arma::uvec ground_indices) -> arma::vec {
        arma::vec dists(ground_indices.n_elem);
        for (arma::uword k = 0; k < ground_indices.n_elem; ++k)
            dists[k] = static_cast<double>(unit_row - ground_indices[k]);
        return as<arma::vec>(wrap(decay_fn(wrap(dists))));
    };
    qe::UnitNetworks r = qe::accumulate_unit_with_rows(codes, unit_rows, cpp_decay, ordered);
    return List::create(
        _("networks")     = r.networks,
        _("row_networks") = r.row_networks
    );
}

//' Tensor-based multi-modal accumulation for one unit (tma model)
//'
//' Pure-C++ port of tma's apply_tensor().  Accepts 0-based index vectors
//' for sender/receiver/mode dimensions and a pre-converted integer context
//' lookup matrix.
//'
//' @param tensor         Numeric vector (column-major flat tensor)
//' @param dims           Integer vector of tensor dimensions
//' @param dims_sender    0-based sender axis indices
//' @param dims_receiver  0-based receiver axis indices
//' @param dims_mode      0-based mode axis indices
//' @param context_lookup Integer matrix (n_context_rows x n_factors), 0-based
//' @param unit_rows      0-based response-row indices for this unit
//' @param codes          Numeric matrix (n_context_rows x n_codes)
//' @param times          Numeric vector of timestamps per context row
//' @param ordered        TRUE = directed; FALSE = undirected
//' @param misses         TRUE = also return missed co-occurrences: ground code
//'   i present while response code j is absent (directed, n_codes^2, ground
//'   i -> response j at j*n+i; no response self-term)
//' @return List with `connection_counts` (vector) and `row_connection_counts`
//'   (matrix); with `misses = TRUE`, also `miss_counts` (vector) and
//'   `row_miss_counts` (matrix), always directed
//' @export
// [[Rcpp::export]]
List apply_tensor(arma::vec tensor,
                     std::vector<int> dims,
                     std::vector<int> dims_sender,
                     std::vector<int> dims_receiver,
                     std::vector<int> dims_mode,
                     arma::imat context_lookup,
                     std::vector<int> unit_rows,
                     arma::mat codes,
                     arma::vec times,
                     bool ordered = true,
                     bool misses = false) {
    qe::TensorNetworks r = qe::apply_tensor_unit(
        tensor, dims, dims_sender, dims_receiver, dims_mode,
        context_lookup, unit_rows, codes, times, ordered,
        /*return_members=*/false, /*return_misses=*/misses);
    List out = List::create(
        _("connection_counts")     = r.connection_counts,
        _("row_connection_counts") = r.row_connection_counts
    );
    if (misses) {
        out["miss_counts"]     = r.miss_counts;
        out["row_miss_counts"] = r.row_miss_counts;
    }
    return out;
}

//' Apply the TMA window/weight tensor, additionally returning per-response-row
//' window membership.
//'
//' Identical to \code{apply_tensor()} but also returns, for each response
//' (reference) row, the in-window ground rows and their resolved window sizes.
//' Powers the webtool Data View per-modality window-span hover. Kept as a
//' separate export so \code{apply_tensor()}'s ABI (and tma's direct .Call to it)
//' is unchanged.
//'
//' @param tensor         Numeric vector (column-major flat tensor)
//' @param dims           Integer vector of tensor dimensions
//' @param dims_sender    0-based sender axis indices
//' @param dims_receiver  0-based receiver axis indices
//' @param dims_mode      0-based mode axis indices
//' @param context_lookup Integer matrix (n_context_rows x n_factors), 0-based
//' @param unit_rows      0-based response-row indices for this unit
//' @param codes          Numeric matrix (n_context_rows x n_codes)
//' @param times          Numeric vector of timestamps per context row
//' @param ordered        TRUE = directed; FALSE = undirected
//' @return List with \code{connection_counts}, \code{row_connection_counts},
//'   \code{row_window_members} (list of 1-based ground-row index vectors, in
//'   \code{unit_rows} order) and \code{row_window_wins} (parallel resolved
//'   window sizes).
//' @export
// [[Rcpp::export]]
List apply_tensor_members(arma::vec tensor,
                     std::vector<int> dims,
                     std::vector<int> dims_sender,
                     std::vector<int> dims_receiver,
                     std::vector<int> dims_mode,
                     arma::imat context_lookup,
                     std::vector<int> unit_rows,
                     arma::mat codes,
                     arma::vec times,
                     bool ordered = true) {
    qe::TensorNetworks r = qe::apply_tensor_unit(
        tensor, dims, dims_sender, dims_receiver, dims_mode,
        context_lookup, unit_rows, codes, times, ordered, /*return_members=*/true);
    List members(r.row_window_members.size());
    List wins(r.row_window_wins.size());
    for (size_t i = 0; i < r.row_window_members.size(); ++i) {
        IntegerVector iv(r.row_window_members[i].begin(), r.row_window_members[i].end());
        for (R_xlen_t k = 0; k < iv.size(); ++k) iv[k] += 1;   // 0-based -> 1-based
        members[i] = iv;
        wins[i] = NumericVector(r.row_window_wins[i].begin(), r.row_window_wins[i].end());
    }
    return List::create(
        _("connection_counts")     = r.connection_counts,
        _("row_connection_counts") = r.row_connection_counts,
        _("row_window_members")    = members,
        _("row_window_wins")       = wins
    );
}

// Weight model from R: NULL (= "binary"), TRUE/FALSE (legacy binary flag) or a name.
static qe::WeightModel weight_model_from_sexp(SEXP weight) {
    if (Rf_isNull(weight)) return qe::WeightModel::Binary;
    if (TYPEOF(weight) == STRSXP && Rf_length(weight) == 1)
        return qe::weight_model_from_string(Rcpp::as<std::string>(weight));
    if (TYPEOF(weight) == LGLSXP && Rf_length(weight) == 1)
        return qe::weight_model_from_bool(Rcpp::as<bool>(weight));
    Rcpp::stop("`weight` must be TRUE/FALSE or one of 'binary', 'product', 'sqrt', 'log1p'.");
}

//' Finalise per-response-row tensor connections (fold + weight model)
//'
//' The per-line step of tma's aggregation, before any summing: unordered rows
//' are folded to the upper triangle, then the weight model is applied; ordered
//' rows keep the directed n*n layout with the weight applied per cell.
//'
//' Weight models are applied per line, before the unit sum (the stage legacy
//' rENA applies \code{weight.by}): \code{"binary"} clamps each unordered count
//' to 1 (ordered counts are left raw), \code{"product"} keeps the counts,
//' \code{"sqrt"} and \code{"log1p"} (alias \code{"log"}) transform each
//' line's product count.
//'
//' @param row_conn Numeric matrix (n_response_rows x n_codes^2) of per-row
//'   directed counts, i.e. \code{apply_tensor()$row_connection_counts}
//' @param n_codes  Number of codes
//' @param ordered  TRUE = keep directed n*n rows; FALSE = fold to choose(n, 2)
//' @param weight   Weight model name (\code{"binary"}, \code{"product"},
//'   \code{"sqrt"}, \code{"log1p"}), or TRUE/FALSE for the legacy binary flag
//'   (TRUE = \code{"binary"}, FALSE = \code{"product"}). NULL (default) =
//'   \code{"binary"}.
//' @return Numeric matrix (n_response_rows x n_codes^2, or x choose(n_codes, 2))
//' @export
// [[Rcpp::export]]
arma::mat finalize_row_connections(arma::mat row_conn,
                                   int  n_codes,
                                   bool ordered = false,
                                   SEXP weight  = R_NilValue) {
    return qe::finalize_row_connections(row_conn, n_codes, ordered,
                                        weight_model_from_sexp(weight));
}

//' Aggregate per-response-row tensor connections into a unit vector
//'
//' Finalises each row (see \code{finalize_row_connections()}) and sums the
//' rows: tma's aggregation of \code{apply_tensor()$row_connection_counts},
//' with the weight model applied per line before the unit sum.
//'
//' @inheritParams finalize_row_connections
//' @return Numeric vector of length n_codes^2 (ordered) or choose(n_codes, 2)
//' @export
// [[Rcpp::export]]
NumericVector aggregate_row_connections(arma::mat row_conn,
                                        int  n_codes,
                                        bool ordered = false,
                                        SEXP weight  = R_NilValue) {
    arma::rowvec out = qe::aggregate_row_connections(row_conn, n_codes, ordered,
                                                     weight_model_from_sexp(weight));
    return NumericVector(out.begin(), out.end());
}
