/**
 * @file libtma/bind/nanobind.hpp
 * @brief Plain-data packers for libtma results in nanobind (Python) bindings.
 *
 * Results cross the boundary as dicts of numpy arrays, not registered
 * nanobind classes, so extension modules built by different products
 * (qe-tma, qe-ena, ...) can be loaded together without type-registration
 * clashes.  Python-side result classes (tma.libtma.UnitNetworks,
 * TensorNetworks) wrap the dicts to keep attribute access.
 *
 * It also holds the bindings themselves, one registration function per libtma
 * function (def_<name>, plus def_libtma for all of them), so every module that
 * exposes them -- qe-tma's tma._libtma, qe-ena's ena._libena -- shares one
 * definition (signature, defaults, docstring) and picks the ones it needs.
 *
 * Requires nanobind; never included by libtma.hpp.  Array conversion comes
 * from libqe/bind/nanobind.hpp.
 */
#ifndef LIBTMA_BIND_NANOBIND_HPP
#define LIBTMA_BIND_NANOBIND_HPP

#include <libqe/bind/nanobind.hpp>
#include <libtma/accumulation.hpp>

#include <nanobind/stl/string.h>
#include <nanobind/stl/variant.h>
#include <nanobind/stl/vector.h>
#include <string>
#include <variant>
#include <vector>

namespace qe {
namespace bind {
namespace py {

/// {networks (1-D), row_networks (2-D)}
inline ::nanobind::dict to_dict(const qe::UnitNetworks& r) {
    ::nanobind::dict out;
    out["networks"]     = from_rowvec(r.networks);
    out["row_networks"] = from_mat(r.row_networks);
    return out;
}

/// {connection_counts (1-D), row_connection_counts (2-D)}, plus
/// {miss_counts (1-D), row_miss_counts (2-D)} when misses were computed.
inline ::nanobind::dict to_dict(const qe::TensorNetworks& r) {
    ::nanobind::dict out;
    out["connection_counts"]     = from_rowvec(r.connection_counts);
    out["row_connection_counts"] = from_mat(r.row_connection_counts);
    if (r.miss_counts.n_elem > 0) {
        out["miss_counts"]     = from_rowvec(r.miss_counts);
        out["row_miss_counts"] = from_mat(r.row_miss_counts);
    }
    return out;
}

// ── Registration ──────────────────────────────────────────────────────────────
// Moved from qe-tma's python/src/libtma_ext.cpp (itself from qe-lib's qe.cpp).

/// Weight model: a name ("binary" | "product" | "sqrt" | "log1p" | "log") or
/// the legacy bool binary flag (True -> "binary", False -> "product").
inline qe::WeightModel weight_model_from_variant(const std::variant<bool, std::string>& w) {
    return std::holds_alternative<bool>(w)
        ? qe::weight_model_from_bool(std::get<bool>(w))
        : qe::weight_model_from_string(std::get<std::string>(w));
}

inline void def_connection_matrix(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("connection_matrix", [](NpVec ground, NpVec response,
                                                  double response_weight, bool ordered) {
            return from_mat(qe::connection_matrix(
                to_rowvec(ground), to_rowvec(response), response_weight, ordered));
        }, "ground"_a, "response"_a, "response_weight"_a = 1.0, "ordered"_a = true,
            "Connection matrix for one ground+response event pair.\n"
            "ordered=True (directed):  ground→response cross-product\n"
            "ordered=False (undirected): symmetric outer-product");
}

inline void def_accumulate_stanza(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("accumulate_stanza", [](NpMat codes, int window_back,
                                     int window_forward, bool binary, bool ordered) {
            return from_mat(qe::accumulate_stanza(
                to_mat(codes), window_back, window_forward, binary, ordered));
        }, "codes"_a, "window_back"_a = 1, "window_forward"_a = 0, "binary"_a = true,
           "ordered"_a = false,
            "Stanza-window accumulation.\n"
            "ordered=False (default): upper-tri co-occurrences, returns (n_rows × choose_two(n_codes)).\n"
            "ordered=True: directed — focal row as response, prior window rows as ground,\n"
            "returns (n_rows × n_codes²). window_forward is ignored when ordered=True.");
}

inline void def_row_connections(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("row_connections", [](NpMat codes, bool binary) {
            return from_mat(qe::row_connections(to_mat(codes), binary));
        }, "codes"_a, "binary"_a = true,
            "Per-row upper-triangle co-occurrence.\n"
            "Each row is processed independently (no cross-row accumulation).\n"
            "Returns (n_rows × choose_two(n_codes)) matrix.");
}

inline void def_rolling_window_sum(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("rolling_window_sum", [](NpMat codes, int window_size) {
            return from_mat(qe::rolling_window_sum(to_mat(codes), window_size));
        }, "codes"_a, "window_size"_a = 1,
            "Rolling backward window sum of a raw code matrix.\n"
            "Row k = sum of rows [max(0, k - window_size + 1), k].\n"
            "Returns matrix of same shape as input.");
}

inline void def_flat_index(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("flat_index", [](std::vector<int> indices, std::vector<int> dims) {
            return qe::flat_index(indices, dims);
        }, "indices"_a, "dims"_a,
            "Linear index into a column-major multi-dimensional array "
            "(equivalent to sub2ind with Fortran/column-major ordering).");
}

inline void def_accumulate_unit(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("accumulate_unit",
            [](NpMat codes, std::vector<int> unit_rows, nb::object decay_fn, bool ordered) {
                auto cpp_decay = [&decay_fn](arma::vec distances) -> arma::vec {
                    auto np_dist = from_vec(distances);
                    auto result  = decay_fn(np_dist);
                    return to_vec(nb::cast<NpVec>(result));
                };
                return from_rowvec(qe::accumulate_unit(
                    to_mat(codes), unit_rows, cpp_decay, ordered));
            },
            "codes"_a, "unit_rows"_a, "decay_fn"_a, "ordered"_a = false,
            "Ground/response accumulation for one unit (tma model).\n\n"
            "Parameters\n----------\n"
            "codes     : ndarray (n_context_rows x n_codes)\n"
            "unit_rows : list[int]  0-based row indices belonging to this unit\n"
            "decay_fn  : callable(distances: ndarray 1-D) -> ndarray 1-D\n"
            "            Maps distance vector to weight vector.\n"
            "ordered   : bool  True = directed full matrix, False = undirected upper-tri\n\n"
            "Returns ndarray 1-D — flat connection vector.");
}

inline void def_accumulate_unit_with_rows(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("accumulate_unit_with_rows",
            [](NpMat codes, std::vector<int> unit_rows, nb::object decay_fn, bool ordered) {
                auto cpp_decay = [&decay_fn](int unit_row, arma::uvec ground_indices) -> arma::vec {
                    auto np_gi  = from_uvec(ground_indices);
                    auto result = decay_fn(unit_row, np_gi);
                    return to_vec(nb::cast<NpVec>(result));
                };
                return to_dict(qe::accumulate_unit_with_rows(
                    to_mat(codes), unit_rows, cpp_decay, ordered));
            },
            "codes"_a, "unit_rows"_a, "decay_fn"_a, "ordered"_a = false,
            "Ground/response accumulation returning per-row connection data (tma model).\n\n"
            "decay_fn : callable(unit_row: int, ground_indices: ndarray int64 1-D) -> ndarray float64 1-D\n"
            "Returns a dict (UnitNetworks in tma.libtma) with .networks (1-D) and .row_networks (2-D).");
}

inline void def_apply_tensor_unit(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("apply_tensor_unit",
            [](NpVec tensor, std::vector<int> dims,
               std::vector<int> dims_sender, std::vector<int> dims_receiver,
               std::vector<int> dims_mode,
               NpIMat context_lookup, std::vector<int> unit_rows,
               NpMat codes, NpVec times, bool ordered, bool misses) {
                return to_dict(qe::apply_tensor_unit(
                    to_vec(tensor), dims, dims_sender, dims_receiver, dims_mode,
                    to_imat(context_lookup), unit_rows,
                    to_mat(codes), to_vec(times), ordered,
                    /*return_members=*/false, /*return_misses=*/misses));
            },
            "tensor"_a, "dims"_a,
            "dims_sender"_a, "dims_receiver"_a, "dims_mode"_a,
            "context_lookup"_a, "unit_rows"_a,
            "codes"_a, "times"_a, "ordered"_a = true, "misses"_a = false,
            "Tensor-based multi-modal accumulation for one unit (tma model).\n\n"
            "tensor         : ndarray 1-D  flat column-major tensor of weights/windows\n"
            "dims           : list[int]    shape of the tensor\n"
            "dims_sender    : list[int]    tensor axis indices for sender factors\n"
            "dims_receiver  : list[int]    tensor axis indices for receiver factors\n"
            "dims_mode      : list[int]    tensor axis indices for mode factors\n"
            "context_lookup : ndarray int32 2-D  (n_rows x n_factors) factor indices\n"
            "unit_rows      : list[int]    0-based response-row indices for this unit\n"
            "codes          : ndarray 2-D  (n_rows x n_codes) code matrix\n"
            "times          : ndarray 1-D  timestamp per context row\n"
            "ordered        : bool         True = directed, False = undirected upper-tri\n"
            "misses         : bool         True = also return missed co-occurrences (ground i\n"
            "                              present, response j absent; directed p^2 rows)\n\n"
            "Returns a dict (TensorNetworks in tma.libtma) with .connection_counts (1-D) and .row_connection_counts (2-D);\n"
            "with misses=True also .miss_counts (1-D) and .row_miss_counts (2-D).\n\n"
            "Default mode: when dims=[2] and tensor has 2 elements [weight, window], uses\n"
            "a simplified single-weight/window path (equivalent to tma's default tensor).");
}

inline void def_aggregate_row_connections(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("aggregate_row_connections",
            [](NpMat row_conn, int n_codes, bool ordered,
                           std::variant<bool, std::string> weight) {
                return from_rowvec(qe::aggregate_row_connections(
                    to_mat(row_conn), n_codes, ordered, weight_model_from_variant(weight)));
            },
            "row_conn"_a, "n_codes"_a, "ordered"_a = false, "weight"_a = "binary",
            "Aggregate apply_tensor_unit's row_connection_counts into a unit vector,\n"
            "matching tma's R aggregation (as.unordered + colSums.ena.matrix), with the\n"
            "weight model applied per row before the sum (= rENA's weight.by).\n\n"
            "row_conn : ndarray 2-D  (n_response_rows x n_codes^2) per-row directed counts\n"
            "n_codes  : int          number of codes p\n"
            "ordered  : bool         True = directed p^2 rows; False = fold each row to choose(p,2)\n"
            "weight   : str | bool   'binary' (unordered: clamp to 1; ordered: raw counts),\n"
            "                        'product' (raw counts), 'sqrt', 'log1p' (alias 'log');\n"
            "                        a bool is the legacy binary flag (True = 'binary',\n"
            "                        False = 'product')\n\n"
            "Returns a 1-D ndarray of length p^2 (ordered) or choose(p,2) (unordered).");
}

inline void def_finalize_row_connections(::nanobind::module_& m) {
    namespace nb = ::nanobind;
    using namespace ::nanobind::literals;
        m.def("finalize_row_connections",
            [](NpMat row_conn, int n_codes, bool ordered,
                           std::variant<bool, std::string> weight) {
                return from_mat(qe::finalize_row_connections(
                    to_mat(row_conn), n_codes, ordered, weight_model_from_variant(weight)));
            },
            "row_conn"_a, "n_codes"_a, "ordered"_a = false, "weight"_a = "binary",
            "Per-row step of aggregate_row_connections (fold + weight model) without\n"
            "the sum; its rows sum to aggregate_row_connections.\n\n"
            "Returns a 2-D ndarray (n_response_rows x p^2 ordered, or x choose(p,2)).");
}

inline void def_finalize_self_connections(::nanobind::module_& m) {
    using namespace ::nanobind::literals;
        m.def("finalize_self_connections",
            [](NpMat row_conn, int n_codes, bool ordered,
                           std::variant<bool, std::string> weight) {
                return from_mat(qe::finalize_self_connections(
                    to_mat(row_conn), n_codes, ordered, weight_model_from_variant(weight)));
            },
            "row_conn"_a, "n_codes"_a, "ordered"_a = false, "weight"_a = "binary",
            "Per-line self-connections: the diagonal (ground i x response i) of each\n"
            "directed row of apply_tensor_unit's row_connection_counts (or\n"
            "row_miss_counts, for self-misses), weighted per line as\n"
            "finalize_row_connections weights it.\n\n"
            "Returns a 2-D ndarray (n_response_rows x p).");
}

inline void def_aggregate_self_connections(::nanobind::module_& m) {
    using namespace ::nanobind::literals;
        m.def("aggregate_self_connections",
            [](NpMat row_conn, int n_codes, bool ordered,
                           std::variant<bool, std::string> weight) {
                return from_rowvec(qe::aggregate_self_connections(
                    to_mat(row_conn), n_codes, ordered, weight_model_from_variant(weight)));
            },
            "row_conn"_a, "n_codes"_a, "ordered"_a = false, "weight"_a = "binary",
            "finalize_self_connections summed over the lines.\n\n"
            "Returns a 1-D ndarray of length p.");
}

/// {connection_counts, row_connection_counts, self_connection_counts,
///  row_self_connection_counts} plus the four miss fields when computed.
inline ::nanobind::dict to_dict(const qe::UnitAccumulation& r) {
    ::nanobind::dict out;
    out["connection_counts"]          = from_rowvec(r.connection_counts);
    out["row_connection_counts"]      = from_mat(r.row_connection_counts);
    out["self_connection_counts"]     = from_rowvec(r.self_connection_counts);
    out["row_self_connection_counts"] = from_mat(r.row_self_connection_counts);
    if (r.miss_counts.n_elem > 0) {
        out["miss_counts"]          = from_rowvec(r.miss_counts);
        out["row_miss_counts"]      = from_mat(r.row_miss_counts);
        out["self_miss_counts"]     = from_rowvec(r.self_miss_counts);
        out["row_self_miss_counts"] = from_mat(r.row_self_miss_counts);
    }
    return out;
}

inline void def_weight_rows(::nanobind::module_& m) {
    using namespace ::nanobind::literals;
        m.def("weight_rows",
            [](NpMat rows, bool ordered, std::variant<bool, std::string> weight) {
                return from_mat(qe::weight_rows(to_mat(rows), ordered, weight_model_from_variant(weight)));
            },
            "rows"_a, "ordered"_a = false, "weight"_a = "binary",
            "Apply the weight model per line to already-finalised rows (folded when\n"
            "unordered), e.g. accumulate_unit_networks' raw row_connection_counts.\n\n"
            "Returns a 2-D ndarray of the same shape.");
}

inline void def_accumulate_unit_networks(::nanobind::module_& m) {
    using namespace ::nanobind::literals;
        m.def("accumulate_unit_networks",
            [](NpVec tensor, std::vector<int> dims,
               std::vector<int> dims_sender, std::vector<int> dims_receiver,
               std::vector<int> dims_mode,
               NpIMat context_lookup, std::vector<int> unit_rows,
               NpMat codes, NpVec times, bool ordered,
               std::variant<bool, std::string> weight, bool misses) {
                return to_dict(qe::accumulate_unit_networks(
                    to_vec(tensor), dims, dims_sender, dims_receiver, dims_mode,
                    to_imat(context_lookup), unit_rows,
                    to_mat(codes), to_vec(times), ordered,
                    weight_model_from_variant(weight), misses));
            },
            "tensor"_a, "dims"_a,
            "dims_sender"_a, "dims_receiver"_a, "dims_mode"_a,
            "context_lookup"_a, "unit_rows"_a,
            "codes"_a, "times"_a, "ordered"_a = false, "weight"_a = "binary",
            "misses"_a = true,
            "One unit's networks end to end: apply_tensor_unit, then fold (unordered),\n"
            "per-line weight model, unit sum and the self-connection diagonal, for hits\n"
            "and (misses=True, default) missed co-occurrences.\n\n"
            "Returns a dict with connection_counts, row_connection_counts (raw; folded\n"
            "when unordered), self_connection_counts, row_self_connection_counts, and\n"
            "with misses miss_counts, row_miss_counts, self_miss_counts,\n"
            "row_self_miss_counts.");
}

/// All of libtma's functions.
inline void def_libtma(::nanobind::module_& m) {
    def_connection_matrix(m);
    def_accumulate_stanza(m);
    def_row_connections(m);
    def_rolling_window_sum(m);
    def_flat_index(m);
    def_accumulate_unit(m);
    def_accumulate_unit_with_rows(m);
    def_apply_tensor_unit(m);
    def_aggregate_row_connections(m);
    def_finalize_row_connections(m);
    def_finalize_self_connections(m);
    def_aggregate_self_connections(m);
    def_accumulate_unit_networks(m);
    def_weight_rows(m);
}

} // namespace py
} // namespace bind
} // namespace qe

#endif // LIBTMA_BIND_NANOBIND_HPP
