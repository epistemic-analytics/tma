// libtma Julia bindings via CxxWrap.jl (the TMA.jl package).
//
// libtma is the accumulation C++ layer (inst/include/libtma): stanza windows,
// context tensors, row connections and weight models.  These methods moved
// from LibQE.jl's libqe_julia.cpp (libqe's phase 4b split) with the same
// signatures and bodies, except that results are plain tuples
// (libtma/bind/cxxwrap.hpp) instead of add_type structs, so this module loads
// alongside other products' modules.  src/TMA.jl turns them into NamedTuples.
//
// Julia matrices are column-major, like Armadillo's, so inputs are zero-copy
// views (view_mat); results are copied out once to std::vector<double>.
//
// accumulate_unit / accumulate_unit_with_rows take a Julia decay function as a
// raw jl_value_t* (CxxWrap 0.15 has no JuliaFunction converter) and call it
// through Julia's C API.

#include <jlcxx/jlcxx.hpp>
#include <jlcxx/array.hpp>
#include <jlcxx/stl.hpp>

#include <armadillo>
#include <libtma/accumulation.hpp>
#include <libqe/bind/cxxwrap.hpp>
#include <libtma/bind/cxxwrap.hpp>

#include <stdexcept>
#include <string>
#include <vector>

// view_mat / pack / copy_imat / unpack_jl_vec (libqe/bind/cxxwrap.hpp) and
// to_tuple for libtma results (libtma/bind/cxxwrap.hpp).
using namespace qe::bind::jl;

JLCXX_MODULE define_julia_module(jlcxx::Module& mod) {

    // ── Accumulation ──────────────────────────────────────────────────────────

    mod.method("connection_matrix",
        [](jlcxx::ArrayRef<double> ground, int32_t gn,
           jlcxx::ArrayRef<double> resp,   int32_t rn,
           double response_weight, bool ordered) -> std::vector<double> {
            arma::rowvec g(ground.data(), gn, false, true);
            arma::rowvec r(resp.data(),   rn, false, true);
            return pack(qe::connection_matrix(g, r, response_weight, ordered));
        });

    mod.method("accumulate_stanza",
        [](jlcxx::ArrayRef<double> codes, int32_t rows, int32_t cols,
           int32_t window_back, int32_t window_forward, bool binary, bool ordered)
         -> std::vector<double> {
            return pack(qe::accumulate_stanza(view_mat(codes, rows, cols),
                                          window_back, window_forward, binary, ordered));
        });

    mod.method("row_connections",
        [](jlcxx::ArrayRef<double> codes, int32_t rows, int32_t cols, bool binary)
         -> std::vector<double> {
            return pack(qe::row_connections(view_mat(codes, rows, cols), binary));
        });

    mod.method("rolling_window_sum",
        [](jlcxx::ArrayRef<double> codes, int32_t rows, int32_t cols, int32_t window_size)
         -> std::vector<double> {
            return pack(qe::rolling_window_sum(view_mat(codes, rows, cols), window_size));
        });

    mod.method("flat_index",
        [](const std::vector<int32_t>& indices, const std::vector<int32_t>& dims) -> int32_t {
            std::vector<int> idx(indices.begin(), indices.end());
            std::vector<int> d(dims.begin(),    dims.end());
            return static_cast<int32_t>(qe::flat_index(idx, d));
        });

    // ── Accumulation — callback-based ─────────────────────────────────────────
    //
    // accumulate_unit and accumulate_unit_with_rows accept a Julia function as
    // the decay_fn argument.  The Julia function receives a Vector{Float64} of
    // distances and must return a Vector{Float64} of weights.
    //
    // CxxWrap 0.15 removed the automatic type converter for jlcxx::JuliaFunction,
    // so we accept jl_value_t* directly and invoke the callback via Julia's C API.
    // call_jl_fn_vec() allocates a temporary Julia Vector{Float64}, fills it from
    // an arma::vec, calls the function, and returns the raw jl_value_t* result.

    mod.method("accumulate_unit",
        [](jlcxx::ArrayRef<double> codes, int32_t rows, int32_t cols,
           const std::vector<int32_t>& unit_rows_i32,
           jl_value_t* decay_fn,
           bool ordered) -> std::vector<double> {

            std::vector<int> unit_rows(unit_rows_i32.begin(), unit_rows_i32.end());

            auto cpp_decay = [decay_fn](arma::vec distances) -> arma::vec {
                // Wrap the arma::vec's buffer as a Julia Vector{Float64} — zero copy.
                // Julia does not own the buffer (own_buffer=0); distances outlives the call.
                jl_value_t* arr_type = jl_apply_array_type(
                    (jl_value_t*)jl_float64_type, 1);
                jl_array_t* arr = jl_ptr_to_array_1d(
                    arr_type,
                    const_cast<double*>(distances.memptr()),
                    static_cast<size_t>(distances.n_elem), 0);
                jl_value_t* result = nullptr;
                // Root both arr and result so GC cannot collect result after jl_call1.
                JL_GC_PUSH2((jl_value_t**)&arr, &result);
                result = jl_call1(decay_fn, (jl_value_t*)arr);
                arma::vec out = unpack_jl_vec(result);
                JL_GC_POP();
                return out;
            };

            return pack(qe::accumulate_unit(view_mat(codes, rows, cols),
                                            unit_rows, cpp_decay, ordered));
        });

    mod.method("accumulate_unit_with_rows",
        [](jlcxx::ArrayRef<double> codes, int32_t rows, int32_t cols,
           const std::vector<int32_t>& unit_rows_i32,
           jl_value_t* decay_fn,
           bool ordered) -> UnitNetworksTuple {

            std::vector<int> unit_rows(unit_rows_i32.begin(), unit_rows_i32.end());

            auto cpp_decay = [decay_fn](int unit_row, arma::uvec ground_indices) -> arma::vec {
                arma::vec distances(ground_indices.n_elem);
                for (arma::uword k = 0; k < ground_indices.n_elem; ++k)
                    distances[k] = static_cast<double>(
                        unit_row - static_cast<int>(ground_indices[k]));
                jl_value_t* arr_type = jl_apply_array_type(
                    (jl_value_t*)jl_float64_type, 1);
                jl_array_t* arr = jl_ptr_to_array_1d(
                    arr_type,
                    const_cast<double*>(distances.memptr()),
                    static_cast<size_t>(distances.n_elem), 0);
                jl_value_t* result = nullptr;
                // Root both arr and result so GC cannot collect result after jl_call1.
                JL_GC_PUSH2((jl_value_t**)&arr, &result);
                result = jl_call1(decay_fn, (jl_value_t*)arr);
                arma::vec out = unpack_jl_vec(result);
                JL_GC_POP();
                return out;
            };

            // LibQE.jl repacked this into the ENA node-positions struct; a
            // plain tuple keeps TMA.jl independent of libena.
            return to_tuple(qe::accumulate_unit_with_rows(
                view_mat(codes, rows, cols), unit_rows, cpp_decay, ordered));
        });

    // accumulate_tensor_unit — tma tensor-based accumulation for one unit.
    //
    // context_lookup is passed as a flat Int32 vector (column-major, Julia-native)
    // plus cl_rows/cl_cols.  All index vectors are Int32; copied to std::vector<int>
    // because qe::apply_tensor_unit takes std::vector<int>.
    auto tensor_unit = [](jlcxx::ArrayRef<double> tensor_ref,
           const std::vector<int32_t>& dims_i32,
           const std::vector<int32_t>& dims_sender_i32,
           const std::vector<int32_t>& dims_receiver_i32,
           const std::vector<int32_t>& dims_mode_i32,
           jlcxx::ArrayRef<int32_t> ctx_ref, int32_t cl_rows, int32_t cl_cols,
           const std::vector<int32_t>& unit_rows_i32,
           jlcxx::ArrayRef<double> codes_ref, int32_t rows, int32_t cols,
           jlcxx::ArrayRef<double> times_ref,
           bool ordered, bool misses) -> qe::TensorNetworks {

            arma::vec tensor(tensor_ref.data(), tensor_ref.size(), false, true);

            std::vector<int> dims(dims_i32.begin(), dims_i32.end());
            std::vector<int> dims_sender(dims_sender_i32.begin(), dims_sender_i32.end());
            std::vector<int> dims_receiver(dims_receiver_i32.begin(), dims_receiver_i32.end());
            std::vector<int> dims_mode(dims_mode_i32.begin(), dims_mode_i32.end());
            std::vector<int> unit_rows(unit_rows_i32.begin(), unit_rows_i32.end());

            arma::imat context_lookup = copy_imat(ctx_ref, cl_rows, cl_cols);
            arma::mat  codes          = view_mat(codes_ref, rows, cols);
            arma::vec  times(times_ref.data(), times_ref.size(), false, true);

            return qe::apply_tensor_unit(
                tensor, dims, dims_sender, dims_receiver, dims_mode,
                context_lookup, unit_rows, codes, times, ordered,
                /*return_members=*/false, /*return_misses=*/misses);
        };

    mod.method("apply_tensor_unit",
        [tensor_unit](jlcxx::ArrayRef<double> tensor, const std::vector<int32_t>& dims,
           const std::vector<int32_t>& dims_sender, const std::vector<int32_t>& dims_receiver,
           const std::vector<int32_t>& dims_mode,
           jlcxx::ArrayRef<int32_t> ctx, int32_t cl_rows, int32_t cl_cols,
           const std::vector<int32_t>& unit_rows,
           jlcxx::ArrayRef<double> codes, int32_t rows, int32_t cols,
           jlcxx::ArrayRef<double> times, bool ordered) -> TensorNetworksTuple {
            return to_tuple(tensor_unit(tensor, dims, dims_sender, dims_receiver, dims_mode,
                ctx, cl_rows, cl_cols, unit_rows, codes, rows, cols, times, ordered, false));
        });

    // apply_tensor_unit_misses — same arguments; also returns missed
    // co-occurrences (ground i present, response j absent; directed n² rows).
    // A separate method so apply_tensor_unit's tuple shape is unchanged.
    mod.method("apply_tensor_unit_misses",
        [tensor_unit](jlcxx::ArrayRef<double> tensor, const std::vector<int32_t>& dims,
           const std::vector<int32_t>& dims_sender, const std::vector<int32_t>& dims_receiver,
           const std::vector<int32_t>& dims_mode,
           jlcxx::ArrayRef<int32_t> ctx, int32_t cl_rows, int32_t cl_cols,
           const std::vector<int32_t>& unit_rows,
           jlcxx::ArrayRef<double> codes, int32_t rows, int32_t cols,
           jlcxx::ArrayRef<double> times, bool ordered) -> TensorMissesTuple {
            return to_misses_tuple(tensor_unit(tensor, dims, dims_sender, dims_receiver, dims_mode,
                ctx, cl_rows, cl_cols, unit_rows, codes, rows, cols, times, ordered, true));
        });

    // ── Weight models (not bound by LibQE.jl; added with the move) ────────────
    // `weight` is a model name: "binary" | "product" | "sqrt" | "log1p" ("log").
    // The Julia wrapper maps the legacy Bool flag to "binary" / "product".

    mod.method("aggregate_row_connections",
        [](jlcxx::ArrayRef<double> row_conn, int32_t rows, int32_t cols,
           int32_t n_codes, bool ordered, const std::string& weight)
         -> std::vector<double> {
            return pack(qe::aggregate_row_connections(
                view_mat(row_conn, rows, cols), n_codes, ordered,
                qe::weight_model_from_string(weight)));
        });

    mod.method("finalize_row_connections",
        [](jlcxx::ArrayRef<double> row_conn, int32_t rows, int32_t cols,
           int32_t n_codes, bool ordered, const std::string& weight)
         -> std::vector<double> {
            return pack(qe::finalize_row_connections(
                view_mat(row_conn, rows, cols), n_codes, ordered,
                qe::weight_model_from_string(weight)));
        });

    // Self-connections: each directed row's diagonal (ground i x response i).
    mod.method("finalize_self_connections",
        [](jlcxx::ArrayRef<double> row_conn, int32_t rows, int32_t cols,
           int32_t n_codes, bool ordered, const std::string& weight)
         -> std::vector<double> {
            return pack(qe::finalize_self_connections(
                view_mat(row_conn, rows, cols), n_codes, ordered,
                qe::weight_model_from_string(weight)));
        });

    mod.method("aggregate_self_connections",
        [](jlcxx::ArrayRef<double> row_conn, int32_t rows, int32_t cols,
           int32_t n_codes, bool ordered, const std::string& weight)
         -> std::vector<double> {
            return pack(qe::aggregate_self_connections(
                view_mat(row_conn, rows, cols), n_codes, ordered,
                qe::weight_model_from_string(weight)));
        });
}
