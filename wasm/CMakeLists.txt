cmake_minimum_required(VERSION 3.18)
project(tma_wasm_libtma LANGUAGES CXX)

# ── libtma + libqe headers ────────────────────────────────────────────────────
# scripts/sync-headers.sh (run by scripts/build.sh) vendors both into
# wasm/include/: libtma from inst/include/libtma, libqe from Conan.
set(HEADER_DIR "${CMAKE_CURRENT_SOURCE_DIR}/include")
if(NOT EXISTS "${HEADER_DIR}/libtma/libtma.hpp"
   OR NOT EXISTS "${HEADER_DIR}/libqe/bind/emscripten.hpp")
    message(FATAL_ERROR "libtma / libqe headers not found in wasm/include/. "
            "Build with `sh wasm/scripts/build.sh`, or run "
            "`sh scripts/sync-headers.sh` from the tma repo root first.")
endif()

# ── Armadillo (provided by Conan) ─────────────────────────────────────────────
find_package(Armadillo REQUIRED)

# ── WASM module ───────────────────────────────────────────────────────────────
add_executable(libtma csrc/libtma_wasm.cpp)

target_include_directories(libtma PRIVATE
    ${HEADER_DIR}
    ${ARMADILLO_INCLUDE_DIRS}
)

target_link_libraries(libtma PRIVATE ${ARMADILLO_LIBRARIES})

# Disable BLAS/LAPACK — Armadillo falls back to its own built-in routines,
# which is the only path that works without external symbols in WASM.
target_compile_definitions(libtma PRIVATE
    ARMA_DONT_USE_BLAS
    ARMA_DONT_USE_LAPACK
)

# ── Emscripten link flags (as libqe-wasm's) ───────────────────────────────────
# MODULARIZE=1 + EXPORT_ES6=1 — an ES module exporting the factory function
# --bind                      — enable Embind
# ALLOW_MEMORY_GROWTH         — needed for dynamic Armadillo allocations
# ENVIRONMENT                 — target web + node
# target_link_options() (not the LINK_FLAGS property) keeps each flag a
# separate token on the linker command line.
set_target_properties(libtma PROPERTIES SUFFIX ".js")

target_link_options(libtma PRIVATE
    -sMODULARIZE=1
    -sEXPORT_ES6=1
    -sEXPORT_NAME=LibTMA
    -sWASM=1
    -sALLOW_MEMORY_GROWTH=1
    "-sENVIRONMENT=web,node"
    --bind
    -O3
)

# ── output to dist/ ───────────────────────────────────────────────────────────
install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/libtma.js
    ${CMAKE_CURRENT_BINARY_DIR}/libtma.wasm
    DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/dist
)
