// libtma WebAssembly bindings via Emscripten Embind (@qe-libs/tma-wasm).
//
// libtma is the accumulation C++ layer (inst/include/libtma): stanza windows,
// context tensors, row connections and weight models.  The bindings moved from
// libqe-wasm (libqe's phase 4b split) with the same signatures and bodies;
// they live in libtma/bind/emscripten.hpp so rena-wasm can register them too.
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

// The bindings, result packing (to_js) and register_libtma live in
// libtma/bind/emscripten.hpp, shared with rena-wasm.
using namespace qe::bind::js;

// ── Embind registrations ──────────────────────────────────────────────────────

EMSCRIPTEN_BINDINGS(libtma) {
    // Every libtma function (libtma/bind/emscripten.hpp).
    register_libtma();
}
