/**
 * @file libtma/bind/cxxwrap.hpp
 * @brief Plain-data packers for libtma results in CxxWrap.jl (Julia) bindings.
 *
 * Results cross the boundary as std::tuple values of std::vector / int32_t,
 * which Julia receives as plain Tuples of CxxWrap's own StdVector type.  No
 * result type is registered with add_type, so modules built by different
 * products (TMA.jl, ENA.jl, ...) can be loaded together: CxxWrap keys
 * registered types by C++ type, and the same packer compiled into two modules
 * would clash.  The Julia wrapper reshapes the tuples into NamedTuples.
 *
 * Matrices travel as (data, rows, cols): data is column-major, Armadillo's
 * and Julia's layout, so `reshape(collect(data), rows, cols)` rebuilds them.
 *
 * Requires CxxWrap (jlcxx, with jlcxx/stl.hpp: link cxxwrap_julia_stl);
 * never included by libtma.hpp.  Array conversion comes from
 * libqe/bind/cxxwrap.hpp.
 */
#ifndef LIBTMA_BIND_CXXWRAP_HPP
#define LIBTMA_BIND_CXXWRAP_HPP

#include <libqe/bind/cxxwrap.hpp>
#include <jlcxx/stl.hpp>
#include <jlcxx/tuple.hpp>

#include <libtma/accumulation.hpp>

#include <cstdint>
#include <tuple>
#include <vector>

namespace qe {
namespace bind {
namespace jl {

/// (networks, row_networks, rows, cols)
using UnitNetworksTuple = std::tuple<
    std::vector<double>, std::vector<double>, int32_t, int32_t>;

inline UnitNetworksTuple to_tuple(const qe::UnitNetworks& r) {
    return {pack(r.networks), pack(r.row_networks),
            static_cast<int32_t>(r.row_networks.n_rows),
            static_cast<int32_t>(r.row_networks.n_cols)};
}

/// (connection_counts, row_connection_counts, rows, cols)
using TensorNetworksTuple = std::tuple<
    std::vector<double>, std::vector<double>, int32_t, int32_t>;

inline TensorNetworksTuple to_tuple(const qe::TensorNetworks& r) {
    return {pack(r.connection_counts), pack(r.row_connection_counts),
            static_cast<int32_t>(r.row_connection_counts.n_rows),
            static_cast<int32_t>(r.row_connection_counts.n_cols)};
}

/// (connection_counts, row_connection_counts, rows, cols,
///  miss_counts, row_miss_counts) — rows/cols shared by both matrices.
using TensorMissesTuple = std::tuple<
    std::vector<double>, std::vector<double>, int32_t, int32_t,
    std::vector<double>, std::vector<double>>;

inline TensorMissesTuple to_misses_tuple(const qe::TensorNetworks& r) {
    return {pack(r.connection_counts), pack(r.row_connection_counts),
            static_cast<int32_t>(r.row_connection_counts.n_rows),
            static_cast<int32_t>(r.row_connection_counts.n_cols),
            pack(r.miss_counts), pack(r.row_miss_counts)};
}

} // namespace jl
} // namespace bind
} // namespace qe

#endif // LIBTMA_BIND_CXXWRAP_HPP
