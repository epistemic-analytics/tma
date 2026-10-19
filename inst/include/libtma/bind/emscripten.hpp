/**
 * @file libtma/bind/emscripten.hpp
 * @brief Plain-data packers for libtma results in Emscripten (Embind) bindings.
 *
 * Results cross the boundary as plain JS objects, not registered Embind
 * classes, matching libqe-wasm's conventions (and its exact output types):
 *
 *   UnitNetworks   → { networks: rowvec_to_js, row_networks: {data, rows, cols} }
 *   TensorNetworks → { connection_counts: number[],
 *                      row_connection_counts: {data, rows, cols},
 *                      miss_counts?: number[],                     (misses only)
 *                      row_miss_counts?: {data, rows, cols} }      (misses only)
 *
 * It also holds the bindings themselves (namespace qe::bind::js::tma_fn) and
 * one registration function per libtma function (register_<name>, plus
 * register_libtma for all of them), so every module that exposes them --
 * tma-wasm's dist/libtma, rena-wasm's dist/libena -- shares one definition.
 * Call them from an EMSCRIPTEN_BINDINGS block.
 *
 * Requires Emscripten; never included by libtma.hpp.  Array conversion comes
 * from libqe/bind/emscripten.hpp.
 */
#ifndef LIBTMA_BIND_EMSCRIPTEN_HPP
#define LIBTMA_BIND_EMSCRIPTEN_HPP

#include <libqe/bind/emscripten.hpp>
#include <libtma/accumulation.hpp>

#include <emscripten/bind.h>
#include <emscripten/val.h>
#include <string>
#include <vector>

namespace qe {
namespace bind {
namespace js {

inline ::emscripten::val to_js(const qe::UnitNetworks& r) {
    ::emscripten::val result = ::emscripten::val::object();
    result.set("networks",     rowvec_to_js(r.networks));
    result.set("row_networks", mat_to_js(r.row_networks));
    return result;
}

inline ::emscripten::val to_js(const qe::UnitAccumulation& r) {
    ::emscripten::val result = ::emscripten::val::object();
    result.set("connection_counts",          rowvec_to_js(r.connection_counts));
    result.set("row_connection_counts",      mat_to_js(r.row_connection_counts));
    result.set("self_connection_counts",     rowvec_to_js(r.self_connection_counts));
    result.set("row_self_connection_counts", mat_to_js(r.row_self_connection_counts));
    if (r.miss_counts.n_elem > 0) {
        result.set("miss_counts",          rowvec_to_js(r.miss_counts));
        result.set("row_miss_counts",      mat_to_js(r.row_miss_counts));
        result.set("self_miss_counts",     rowvec_to_js(r.self_miss_counts));
        result.set("row_self_miss_counts", mat_to_js(r.row_self_miss_counts));
    }
    return result;
}

inline ::emscripten::val to_js(const qe::TensorNetworks& r) {
    std::vector<double> cc(r.connection_counts.memptr(),
                           r.connection_counts.memptr() + r.connection_counts.n_elem);
    ::emscripten::val result = ::emscripten::val::object();
    result.set("connection_counts",     ::emscripten::val::array(cc.begin(), cc.end()));
    result.set("row_connection_counts", mat_to_js(r.row_connection_counts));
    if (r.miss_counts.n_elem > 0) {
        std::vector<double> mc(r.miss_counts.memptr(),
                               r.miss_counts.memptr() + r.miss_counts.n_elem);
        result.set("miss_counts",     ::emscripten::val::array(mc.begin(), mc.end()));
        result.set("row_miss_counts", mat_to_js(r.row_miss_counts));
    }
    return result;
}


// ── Bindings ──────────────────────────────────────────────────────────────────
// Moved from tma-wasm's csrc/libtma_wasm.cpp (itself from libqe-wasm's
// libqe_wasm.cpp).

namespace tma_fn {

using ::emscripten::val;
using ::emscripten::vecFromJSArray;

// ── Accumulation ─────────────────────────────────────────────────────────────

// connection_matrix(ground, gn, response, rn, response_weight, ordered)
// → { data, rows:n_codes, cols:n_codes }
inline val connection_matrix(const val& ground_data, int gn,
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
inline val accumulate_stanza(const val& data, int rows, int cols,
                              int window_back, int window_forward,
                              bool binary, bool ordered) {
    return mat_to_js(qe::accumulate_stanza(
        js_to_mat(data, rows, cols), window_back, window_forward, binary, ordered));
}

// row_connections(data, rows, cols, binary) → { data, rows, cols }
inline val row_connections(const val& data, int rows, int cols, bool binary) {
    return mat_to_js(qe::row_connections(js_to_mat(data, rows, cols), binary));
}

// rolling_window_sum(data, rows, cols, window_size) → { data, rows, cols }
inline val rolling_window_sum(const val& data, int rows, int cols, int window_size) {
    return mat_to_js(qe::rolling_window_sum(js_to_mat(data, rows, cols), window_size));
}

// flat_index(indices_array, dims_array) → int
inline int flat_index(const val& indices_val, const val& dims_val) {
    return qe::flat_index(
        vecFromJSArray<int>(indices_val),
        vecFromJSArray<int>(dims_val));
}

// accumulate_unit(codes, rows, cols, unit_rows, decay_fn, ordered)
// decay_fn: JS function(Float64Array distances) → Float64Array weights
// → Float64Array  (length choose_two(n_codes) or n_codes²)
inline val accumulate_unit(const val& codes_data, int rows, int cols,
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
inline val accumulate_unit_with_rows(const val& codes_data, int rows, int cols,
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
inline val tensor_unit_impl(
        const val& tensor_val,
        const val& dims_val,
        const val& dims_sender_val,
        const val& dims_receiver_val,
        const val& dims_mode_val,
        const val& context_lookup_val, int cl_rows, int cl_cols,
        const val& unit_rows_val,
        const val& codes_val, int rows, int cols,
        const val& times_val,
        bool ordered,
        bool misses) {

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
        context_lookup, unit_rows, codes_mat, times_vec, ordered,
        /*return_members=*/false, /*return_misses=*/misses));
}

inline val accumulate_tensor_unit(
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
    return tensor_unit_impl(
        tensor_val, dims_val, dims_sender_val, dims_receiver_val, dims_mode_val,
        context_lookup_val, cl_rows, cl_cols, unit_rows_val,
        codes_val, rows, cols, times_val, ordered, /*misses=*/false);
}

// accumulate_tensor_unit_misses(...same arguments as accumulate_tensor_unit...)
// → accumulate_tensor_unit's object plus
//   { miss_counts: number[], row_miss_counts: { data, rows, cols } }
//
// Missed co-occurrences: ground code i present while response code j is absent
// (directed n² rows, ground i → response j at j*n+i; no response self-term).
// A separate export because Embind matches on exact arity, so adding a
// parameter to accumulate_tensor_unit would break existing callers.
inline val accumulate_tensor_unit_misses(
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
    return tensor_unit_impl(
        tensor_val, dims_val, dims_sender_val, dims_receiver_val, dims_mode_val,
        context_lookup_val, cl_rows, cl_cols, unit_rows_val,
        codes_val, rows, cols, times_val, ordered, /*misses=*/true);
}

// Weight model from JS: a name ('binary' | 'product' | 'sqrt' | 'log1p' | 'log')
// or the legacy boolean binary flag (true → 'binary', false → 'product').
inline qe::WeightModel weight_model_from_val(const val& weight) {
    if (weight.isString()) return qe::weight_model_from_string(weight.as<std::string>());
    return qe::weight_model_from_bool(weight.as<bool>());
}

// aggregate_row_connections(row_conn, rows, cols, n_codes, ordered, weight)
//   row_conn — Float64Array, row-major (rows × cols), cols == n_codes²
//              (TensorNetworks.row_connection_counts from accumulate_tensor_unit)
//   weight   — weight-model name, or boolean binary flag (legacy)
// → Float64Array — unit vector, length n_codes² (ordered) or choose_two (unordered)
inline val aggregate_row_connections(
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
inline val finalize_row_connections(
        const val& row_conn_val, int rows, int cols,
        int n_codes, bool ordered, const val& weight) {
    arma::mat row_conn = js_to_mat(row_conn_val, rows, cols);
    return mat_to_js(qe::finalize_row_connections(
        row_conn, n_codes, ordered, weight_model_from_val(weight)));
}

// accumulate_unit_networks(tensor, dims, dims_sender, dims_receiver, dims_mode,
//                          context_lookup, cl_rows, cl_cols, unit_rows,
//                          codes, rows, cols, times, ordered, weight, misses)
// → { connection_counts, row_connection_counts, self_connection_counts,
//     row_self_connection_counts, [miss_counts, row_miss_counts,
//     self_miss_counts, row_self_miss_counts] }
//   One unit's finished networks (fold, per-line weight model, unit sum, self
//   diagonal) in one call; arguments as accumulate_tensor_unit plus weight
//   (model name or legacy boolean) and misses.
inline val accumulate_unit_networks(
        const val& tensor_val,
        const val& dims_val,
        const val& dims_sender_val,
        const val& dims_receiver_val,
        const val& dims_mode_val,
        const val& context_lookup_val, int cl_rows, int cl_cols,
        const val& unit_rows_val,
        const val& codes_val, int rows, int cols,
        const val& times_val,
        bool ordered, const val& weight, bool misses) {
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
    return to_js(qe::accumulate_unit_networks(
        tensor, dims, dims_sender, dims_receiver, dims_mode,
        context_lookup, unit_rows, codes_mat, times_vec,
        ordered, weight_model_from_val(weight), misses));
}

// finalize_self_connections(row_conn, rows, cols, n_codes, ordered, weight)
//   Per-line self-connections: each directed row's diagonal (ground i x
//   response i), weighted per line. Pass row_miss_counts for self-misses.
// → { data: Float64Array, rows, cols } — row-major, cols = n_codes
inline val finalize_self_connections(
        const val& row_conn_val, int rows, int cols,
        int n_codes, bool ordered, const val& weight) {
    arma::mat row_conn = js_to_mat(row_conn_val, rows, cols);
    return mat_to_js(qe::finalize_self_connections(
        row_conn, n_codes, ordered, weight_model_from_val(weight)));
}

// aggregate_self_connections(row_conn, rows, cols, n_codes, ordered, weight)
// → Float64Array — unit self vector, length n_codes
inline val aggregate_self_connections(
        const val& row_conn_val, int rows, int cols,
        int n_codes, bool ordered, const val& weight) {
    arma::mat row_conn = js_to_mat(row_conn_val, rows, cols);
    arma::rowvec out = qe::aggregate_self_connections(
        row_conn, n_codes, ordered, weight_model_from_val(weight));
    std::vector<double> v(out.memptr(), out.memptr() + out.n_elem);
    return val::array(v.begin(), v.end());
}

} // namespace tma_fn

// ── Registration ──────────────────────────────────────────────────────────────

inline void register_connection_matrix() { ::emscripten::function("connection_matrix", &tma_fn::connection_matrix); }
inline void register_accumulate_stanza() { ::emscripten::function("accumulate_stanza", &tma_fn::accumulate_stanza); }
inline void register_row_connections() { ::emscripten::function("row_connections", &tma_fn::row_connections); }
inline void register_rolling_window_sum() { ::emscripten::function("rolling_window_sum", &tma_fn::rolling_window_sum); }
inline void register_flat_index() { ::emscripten::function("flat_index", &tma_fn::flat_index); }
inline void register_accumulate_unit() { ::emscripten::function("accumulate_unit", &tma_fn::accumulate_unit); }
inline void register_accumulate_unit_with_rows() { ::emscripten::function("accumulate_unit_with_rows", &tma_fn::accumulate_unit_with_rows); }
inline void register_accumulate_tensor_unit() { ::emscripten::function("accumulate_tensor_unit", &tma_fn::accumulate_tensor_unit); }
inline void register_accumulate_tensor_unit_misses() { ::emscripten::function("accumulate_tensor_unit_misses", &tma_fn::accumulate_tensor_unit_misses); }
inline void register_aggregate_row_connections() { ::emscripten::function("aggregate_row_connections", &tma_fn::aggregate_row_connections); }
inline void register_finalize_row_connections() { ::emscripten::function("finalize_row_connections", &tma_fn::finalize_row_connections); }
inline void register_finalize_self_connections() { ::emscripten::function("finalize_self_connections", &tma_fn::finalize_self_connections); }
inline void register_accumulate_unit_networks() { ::emscripten::function("accumulate_unit_networks", &tma_fn::accumulate_unit_networks); }
inline void register_aggregate_self_connections() { ::emscripten::function("aggregate_self_connections", &tma_fn::aggregate_self_connections); }

/// All of libtma's functions.
inline void register_libtma() {
    register_connection_matrix();
    register_accumulate_stanza();
    register_row_connections();
    register_rolling_window_sum();
    register_flat_index();
    register_accumulate_unit();
    register_accumulate_unit_with_rows();
    register_accumulate_tensor_unit();
    register_accumulate_tensor_unit_misses();
    register_aggregate_row_connections();
    register_finalize_row_connections();
    register_finalize_self_connections();
    register_aggregate_self_connections();
    register_accumulate_unit_networks();
}

} // namespace js
} // namespace bind
} // namespace qe

#endif // LIBTMA_BIND_EMSCRIPTEN_HPP
