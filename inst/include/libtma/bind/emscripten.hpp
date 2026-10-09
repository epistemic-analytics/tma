/**
 * @file libtma/bind/emscripten.hpp
 * @brief Plain-data packers for libtma results in Emscripten (Embind) bindings.
 *
 * Results cross the boundary as plain JS objects, not registered Embind
 * classes, matching libqe-wasm's conventions (and its exact output types):
 *
 *   UnitNetworks   → { networks: rowvec_to_js, row_networks: {data, rows, cols} }
 *   TensorNetworks → { connection_counts: number[],
 *                      row_connection_counts: {data, rows, cols} }
 *
 * Requires Emscripten; never included by libtma.hpp.  Array conversion comes
 * from libqe/bind/emscripten.hpp.
 */
#ifndef LIBTMA_BIND_EMSCRIPTEN_HPP
#define LIBTMA_BIND_EMSCRIPTEN_HPP

#include <libqe/bind/emscripten.hpp>
#include <libtma/accumulation.hpp>

#include <emscripten/val.h>
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

inline ::emscripten::val to_js(const qe::TensorNetworks& r) {
    std::vector<double> cc(r.connection_counts.memptr(),
                           r.connection_counts.memptr() + r.connection_counts.n_elem);
    ::emscripten::val result = ::emscripten::val::object();
    result.set("connection_counts",     ::emscripten::val::array(cc.begin(), cc.end()));
    result.set("row_connection_counts", mat_to_js(r.row_connection_counts));
    return result;
}

} // namespace js
} // namespace bind
} // namespace qe

#endif // LIBTMA_BIND_EMSCRIPTEN_HPP
