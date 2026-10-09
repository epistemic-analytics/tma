// _libtma — nanobind bindings for libtma, the accumulation C++ layer
// (inst/include/libtma).
//
// One flat module, imported by tma/libtma.py, which wraps the plain-data
// results (dicts of numpy arrays, from libtma/bind/nanobind.hpp) in Python
// result classes.  Function bodies and docstrings moved from qe-lib's qe.cpp
// accumulation submodule (libqe's phase 4b split); results are dicts instead
// of registered classes so this module can be loaded alongside other
// products' extensions.
//
// All matrix arguments are accepted as 2-D numpy float64 arrays (C-contiguous).
// All vector arguments are accepted as 1-D numpy float64 arrays.
// Return values are always freshly allocated numpy arrays (owned by Python).

#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>
#include <nanobind/stl/string.h>
#include <nanobind/stl/vector.h>
#include <nanobind/stl/variant.h>
#include <string>
#include <variant>
#include <vector>

#include <armadillo>
#include <libqe/bind/nanobind.hpp>
#include <libtma/accumulation.hpp>
#include <libtma/bind/nanobind.hpp>

namespace nb = nanobind;
using namespace nb::literals;

// to_mat / from_mat / ... (libqe/bind/nanobind.hpp) and to_dict for libtma
// results (libtma/bind/nanobind.hpp).
using namespace qe::bind::py;

NB_MODULE(_libtma, m) {
    m.doc() = "_libtma — nanobind bindings for libtma (TMA / ENA network accumulation)";

    m.def("connection_matrix", [](NpVec ground, NpVec response,
                                              double response_weight, bool ordered) {
        return from_mat(qe::connection_matrix(
            to_rowvec(ground), to_rowvec(response), response_weight, ordered));
    }, "ground"_a, "response"_a, "response_weight"_a = 1.0, "ordered"_a = true,
        "Connection matrix for one ground+response event pair.\n"
        "ordered=True (directed):  ground→response cross-product\n"
        "ordered=False (undirected): symmetric outer-product");

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

    m.def("row_connections", [](NpMat codes, bool binary) {
        return from_mat(qe::row_connections(to_mat(codes), binary));
    }, "codes"_a, "binary"_a = true,
        "Per-row upper-triangle co-occurrence.\n"
        "Each row is processed independently (no cross-row accumulation).\n"
        "Returns (n_rows × choose_two(n_codes)) matrix.");

    m.def("rolling_window_sum", [](NpMat codes, int window_size) {
        return from_mat(qe::rolling_window_sum(to_mat(codes), window_size));
    }, "codes"_a, "window_size"_a = 1,
        "Rolling backward window sum of a raw code matrix.\n"
        "Row k = sum of rows [max(0, k - window_size + 1), k].\n"
        "Returns matrix of same shape as input.");

    m.def("flat_index", [](std::vector<int> indices, std::vector<int> dims) {
        return qe::flat_index(indices, dims);
    }, "indices"_a, "dims"_a,
        "Linear index into a column-major multi-dimensional array "
        "(equivalent to sub2ind with Fortran/column-major ordering).");

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

    m.def("apply_tensor_unit",
        [](NpVec tensor, std::vector<int> dims,
           std::vector<int> dims_sender, std::vector<int> dims_receiver,
           std::vector<int> dims_mode,
           NpIMat context_lookup, std::vector<int> unit_rows,
           NpMat codes, NpVec times, bool ordered) {
            return to_dict(qe::apply_tensor_unit(
                to_vec(tensor), dims, dims_sender, dims_receiver, dims_mode,
                to_imat(context_lookup), unit_rows,
                to_mat(codes), to_vec(times), ordered));
        },
        "tensor"_a, "dims"_a,
        "dims_sender"_a, "dims_receiver"_a, "dims_mode"_a,
        "context_lookup"_a, "unit_rows"_a,
        "codes"_a, "times"_a, "ordered"_a = true,
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
        "ordered        : bool         True = directed, False = undirected upper-tri\n\n"
        "Returns a dict (TensorNetworks in tma.libtma) with .connection_counts (1-D) and .row_connection_counts (2-D).\n\n"
        "Default mode: when dims=[2] and tensor has 2 elements [weight, window], uses\n"
        "a simplified single-weight/window path (equivalent to tma's default tensor).");

    // Weight model: a name ("binary" | "product" | "sqrt" | "log1p" | "log") or
    // the legacy bool binary flag (True -> "binary", False -> "product").
    auto weight_model = [](const std::variant<bool, std::string>& w) {
        return std::holds_alternative<bool>(w)
            ? qe::weight_model_from_bool(std::get<bool>(w))
            : qe::weight_model_from_string(std::get<std::string>(w));
    };

    m.def("aggregate_row_connections",
        [weight_model](NpMat row_conn, int n_codes, bool ordered,
                       std::variant<bool, std::string> weight) {
            return from_rowvec(qe::aggregate_row_connections(
                to_mat(row_conn), n_codes, ordered, weight_model(weight)));
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

    m.def("finalize_row_connections",
        [weight_model](NpMat row_conn, int n_codes, bool ordered,
                       std::variant<bool, std::string> weight) {
            return from_mat(qe::finalize_row_connections(
                to_mat(row_conn), n_codes, ordered, weight_model(weight)));
        },
        "row_conn"_a, "n_codes"_a, "ordered"_a = false, "weight"_a = "binary",
        "Per-row step of aggregate_row_connections (fold + weight model) without\n"
        "the sum; its rows sum to aggregate_row_connections.\n\n"
        "Returns a 2-D ndarray (n_response_rows x p^2 ordered, or x choose(p,2)).");
}
