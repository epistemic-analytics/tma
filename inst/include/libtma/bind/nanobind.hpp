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
 * Requires nanobind; never included by libtma.hpp.  Array conversion comes
 * from libqe/bind/nanobind.hpp.
 */
#ifndef LIBTMA_BIND_NANOBIND_HPP
#define LIBTMA_BIND_NANOBIND_HPP

#include <libqe/bind/nanobind.hpp>
#include <libtma/accumulation.hpp>

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

/// {connection_counts (1-D), row_connection_counts (2-D)}
inline ::nanobind::dict to_dict(const qe::TensorNetworks& r) {
    ::nanobind::dict out;
    out["connection_counts"]     = from_rowvec(r.connection_counts);
    out["row_connection_counts"] = from_mat(r.row_connection_counts);
    return out;
}

} // namespace py
} // namespace bind
} // namespace qe

#endif // LIBTMA_BIND_NANOBIND_HPP
