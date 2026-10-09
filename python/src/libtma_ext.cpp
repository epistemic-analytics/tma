// _libtma — nanobind bindings for libtma, the accumulation C++ layer
// (inst/include/libtma).
//
// One flat module, imported by tma/libtma.py, which wraps the plain-data
// results (dicts of numpy arrays, from libtma/bind/nanobind.hpp) in Python
// result classes.  The bindings (bodies, defaults, docstrings; moved from
// qe-lib's qe.cpp in libqe's phase 4b split) live in libtma/bind/nanobind.hpp
// as def_<name>(), shared with qe-ena's ena._libena; results are dicts instead
// of registered classes so this module can be loaded alongside other
// products' extensions.
//
// All matrix arguments are accepted as 2-D numpy float64 arrays (C-contiguous).
// All vector arguments are accepted as 1-D numpy float64 arrays.
// Return values are always freshly allocated numpy arrays (owned by Python).

#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>

#include <armadillo>
#include <libqe/bind/nanobind.hpp>
#include <libtma/accumulation.hpp>
#include <libtma/bind/nanobind.hpp>

namespace nb = nanobind;
using namespace nb::literals;

// def_libtma and the to_dict packers (libtma/bind/nanobind.hpp).
using namespace qe::bind::py;

NB_MODULE(_libtma, m) {
    m.doc() = "_libtma — nanobind bindings for libtma (TMA / ENA network accumulation)";

    // Every libtma function (libtma/bind/nanobind.hpp).
    def_libtma(m);
}
