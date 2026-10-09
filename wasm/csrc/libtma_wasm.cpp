// libtma WebAssembly bindings via Emscripten Embind (@qe-libs/tma-wasm).
//
// libtma is the accumulation C++ layer (inst/include/libtma): stanza windows,
// context tensors, row connections and weight models.  These functions moved
// here from libqe-wasm (libqe's phase 4b split) with the same signatures and
// bodies; results are plain JS objects packed by libtma/bind/emscripten.hpp.
//
// Matrix convention (as libqe-wasm): every matrix argument is a flat
// Float64Array (row-major) plus explicit rows/cols; results are plain JS
// objects, matrices as { data: Float64Array, rows: number, cols: number }.
//
// Disable BLAS/LAPACK — Armadillo falls back to its own built-in routines,
// which compile cleanly with Emscripten.

#ifndef ARMA_DONT_USE_BLAS
#  define ARMA_DONT_USE_BLAS
#endif
#ifndef ARMA_DONT_USE_LAPACK
#  define ARMA_DONT_USE_LAPACK
#endif

#include <armadillo>
#include <libtma/accumulation.hpp>
#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <libqe/bind/emscripten.hpp>
#include <libtma/bind/emscripten.hpp>
#include <vector>
#include <string>

using namespace emscripten;

// Array conversion (js_to_mat, js_to_imat, mat_to_js, …) comes from
// libqe/bind/emscripten.hpp; result packing (to_js) from
// libtma/bind/emscripten.hpp.
using namespace qe::bind::js;

// ── Accumulation ─────────────────────────────────────────────────────────────

// connection_matrix(ground, gn, response, rn, response_weight, ordered)
// → { data, rows:n_codes, cols:n_codes }
static val connection_matrix(const val& ground_data, int gn,
                              const val& response_data, int rn,
                              double response_weight, bool ordered) {
    std::vector<double> gv = vecFromJSArray<double>(ground_data);
    std::vector<double> rv = vecFromJSArray<double>(response_data);
    arma::rowvec g(gv.data(), gn);
    arma::rowvec r(rv.data(), rn);
    return mat_to_js(qe::connection_matrix(g, r, response_weight, ordered));
}

// accumulate_stanza(data, rows, cols, window_back, window_forward, binary, ordered)
// ordered=false → { data, rows, choose_two(cols) }
// ordered=true  → { data, rows, cols² }
static val accumulate_stanza(const val& data, int rows, int cols,
                              int window_back, int window_forward,
                              bool binary, bool ordered) {
    return mat_to_js(qe::accumulate_stanza(
        js_to_mat(data, rows, cols), window_back, window_forward, binary, ordered));
}

// row_connections(data, rows, cols, binary) → { data, rows, cols }
static val row_connections(const val& data, int rows, int cols, bool binary) {
    return mat_to_js(qe::row_connections(js_to_mat(data, rows, cols), binary));
}

// rolling_window_sum(data, rows, cols, window_size) → { data, rows, cols }
static val rolling_window_sum(const val& data, int rows, int cols, int window_size) {
    return mat_to_js(qe::rolling_window_sum(js_to_mat(data, rows, cols), window_size));
}

// flat_index(indices_array, dims_array) → int
static int flat_index(const val& indices_val, const val& dims_val) {
    return qe::flat_index(
        vecFromJSArray<int>(indices_val),
        vecFromJSArray<int>(dims_val));
}

// accumulate_unit(codes, rows, cols, unit_rows, decay_fn, ordered)
// decay_fn: JS function(Float64Array distances) → Float64Array weights
// → Float64Array  (length choose_two(n_codes) or n_codes²)
static val accumulate_unit(const val& codes_data, int rows, int cols,
                            const val& unit_rows_val,
                            const val& decay_fn_js,
                            bool ordered) {
    std::vector<int> unit_rows = vecFromJSArray<int>(unit_rows_val);

    auto cpp_decay = [&decay_fn_js](arma::vec distances) -> arma::vec {
        std::vector<double> dv(distances.memptr(), distances.memptr() + distances.n_elem);
        val js_result = decay_fn_js(val::array(dv.begin(), dv.end()));
        std::vector<double> wv = vecFromJSArray<double>(js_result);
        return arma::vec(wv.data(), wv.size());
    };

    return rowvec_to_js(qe::accumulate_unit(
        js_to_mat(codes_data, rows, cols), unit_rows, cpp_decay, ordered));
}

// accumulate_unit_with_rows(codes, rows, cols, unit_rows, decay_fn, ordered)
// → { networks: Float64Array, row_networks: matObj }
static val accumulate_unit_with_rows(const val& codes_data, int rows, int cols,
                                      const val& unit_rows_val,
                                      const val& decay_fn_js,
                                      bool ordered) {
    std::vector<int> unit_rows = vecFromJSArray<int>(unit_rows_val);

    auto cpp_decay = [&decay_fn_js](int unit_row, arma::uvec ground_indices) -> arma::vec {
        arma::vec distances(ground_indices.n_elem);
        for (arma::uword k = 0; k < ground_indices.n_elem; ++k)
            distances[k] = static_cast<double>(unit_row - ground_indices[k]);
        std::vector<double> dv(distances.memptr(), distances.memptr() + distances.n_elem);
        val js_result = decay_fn_js(val::array(dv.begin(), dv.end()));
        std::vector<double> wv = vecFromJSArray<double>(js_result);
        return arma::vec(wv.data(), wv.size());
    };

    return to_js(qe::accumulate_unit_with_rows(
        js_to_mat(codes_data, rows, cols), unit_rows, cpp_decay, ordered));
}

// accumulate_tensor_unit(tensor, dims, dims_sender, dims_receiver, dims_mode,
//                        context_lookup, cl_rows, cl_cols,
//                        unit_rows, codes, rows, cols, times, ordered)
// → { connection_counts: Float64Array,
//     row_connection_counts: { data: Float64Array, rows, cols } }
//
// tensor           — Float64Array, flat column-major, shape described by dims
// dims             — Int32Array  — sizes of each tensor axis
// dims_sender      — Int32Array  — which tensor axes correspond to sender factors
// dims_receiver    — Int32Array  — which tensor axes correspond to receiver factors
// dims_mode        — Int32Array  — which tensor axes correspond to mode factors
// context_lookup   — Int32Array, row-major (n_context_rows × n_factors), 0-based
// cl_rows/cl_cols  — shape of context_lookup
// unit_rows        — Int32Array, 0-based response-row indices for this unit
// codes            — Float64Array, row-major (rows × cols)
// times            — Float64Array, one timestamp per context row
// ordered          — bool (true → directed n²; false → undirected upper-tri)
static val accumulate_tensor_unit(
        const val& tensor_val,
        const val& dims_val,
        const val& dims_sender_val,
        const val& dims_receiver_val,
        const val& dims_mode_val,
        const val& context_lookup_val, int cl_rows, int cl_cols,
        const val& unit_rows_val,
        const val& codes_val, int rows, int cols,
        const val& times_val,
        bool ordered) {

    std::vector<double> tv = vecFromJSArray<double>(tensor_val);
    arma::vec tensor(tv.data(), tv.size());

    std::vector<int> dims          = vecFromJSArray<int>(dims_val);
    std::vector<int> dims_sender   = vecFromJSArray<int>(dims_sender_val);
    std::vector<int> dims_receiver = vecFromJSArray<int>(dims_receiver_val);
    std::vector<int> dims_mode     = vecFromJSArray<int>(dims_mode_val);
    std::vector<int> unit_rows     = vecFromJSArray<int>(unit_rows_val);

    arma::imat context_lookup = js_to_imat(context_lookup_val, cl_rows, cl_cols);
    arma::mat  codes_mat      = js_to_mat(codes_val, rows, cols);

    std::vector<double> timev = vecFromJSArray<double>(times_val);
    arma::vec times_vec(timev.data(), timev.size());

    return to_js(qe::apply_tensor_unit(
        tensor, dims, dims_sender, dims_receiver, dims_mode,
        context_lookup, unit_rows, codes_mat, times_vec, ordered));
}

// Weight model from JS: a name ('binary' | 'product' | 'sqrt' | 'log1p' | 'log')
// or the legacy boolean binary flag (true → 'binary', false → 'product').
static qe::WeightModel weight_model_from_val(const val& weight) {
    if (weight.isString()) return qe::weight_model_from_string(weight.as<std::string>());
    return qe::weight_model_from_bool(weight.as<bool>());
}

// aggregate_row_connections(row_conn, rows, cols, n_codes, ordered, weight)
//   row_conn — Float64Array, row-major (rows × cols), cols == n_codes²
//              (TensorNetworks.row_connection_counts from accumulate_tensor_unit)
//   weight   — weight-model name, or boolean binary flag (legacy)
// → Float64Array — unit vector, length n_codes² (ordered) or choose_two (unordered)
static val aggregate_row_connections(
        const val& row_conn_val, int rows, int cols,
        int n_codes, bool ordered, const val& weight) {
    arma::mat row_conn = js_to_mat(row_conn_val, rows, cols);
    arma::rowvec out = qe::aggregate_row_connections(
        row_conn, n_codes, ordered, weight_model_from_val(weight));
    std::vector<double> v(out.memptr(), out.memptr() + out.n_elem);
    return val::array(v.begin(), v.end());
}

// finalize_row_connections(row_conn, rows, cols, n_codes, ordered, weight)
//   Per-row fold + weight model, before the unit sum.
// → { data: Float64Array, rows, cols } — row-major, cols = n_codes² (ordered)
//   or choose_two (unordered)
static val finalize_row_connections(
        const val& row_conn_val, int rows, int cols,
        int n_codes, bool ordered, const val& weight) {
    arma::mat row_conn = js_to_mat(row_conn_val, rows, cols);
    return mat_to_js(qe::finalize_row_connections(
        row_conn, n_codes, ordered, weight_model_from_val(weight)));
}

// ── Embind registrations ──────────────────────────────────────────────────────

EMSCRIPTEN_BINDINGS(libtma) {
    // Accumulation
    function("connection_matrix",                     &connection_matrix);
    function("accumulate_stanza",                     &accumulate_stanza);
    function("row_connections",                       &row_connections);
    function("rolling_window_sum",                    &rolling_window_sum);
    function("flat_index",                            &flat_index);
    function("accumulate_unit",                       &accumulate_unit);
    function("accumulate_unit_with_rows",             &accumulate_unit_with_rows);
    function("accumulate_tensor_unit",                &accumulate_tensor_unit);
    function("aggregate_row_connections",             &aggregate_row_connections);
    function("finalize_row_connections",              &finalize_row_connections);
}
