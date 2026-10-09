#!/bin/sh
# Build tma-wasm's libtma WASM module (dist/libtma.js + dist/libtma.wasm).
#
# Prerequisites:
#   - emscripten (emcc on PATH)
#   - conan 2.x (for Armadillo; scripts/sync-headers.sh installs its own if
#     none is on PATH, for libqe's headers)
#
# Usage (from the tma repo root or wasm/):
#   sh wasm/scripts/build.sh
#
# cranqe's scripts/wasm/build.sh runs this for C++ wasm packages (it detects
# wasm/CMakeLists.txt), then `npm pack`s the result.

set -e
# WASM = tma/wasm/ (where CMakeLists.txt and conanfile.py live)
WASM="$(cd "$(dirname "$0")/.." && pwd)"
BUILD="${WASM}/build/wasm"

echo "==> vendor libtma + libqe headers into wasm/include"
sh "${WASM}/../scripts/sync-headers.sh"

echo "==> conan install"
conan install "${WASM}" \
    -pr:b=default \
    -pr:h="${WASM}/profiles/wasm" \
    --build=missing \
    --output-folder="${BUILD}"

echo "==> cmake configure"
# Detect Ninja; fall back to Unix Makefiles if not on PATH (emcmake may alter
# PATH, so probe explicitly rather than rely on CMake's auto-detection).
if command -v ninja > /dev/null 2>&1; then
    GENERATOR_FLAGS="-G Ninja"
else
    GENERATOR_FLAGS=""
fi
emcmake cmake -B "${BUILD}" \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_TOOLCHAIN_FILE="${BUILD}/conan_toolchain.cmake" \
    ${GENERATOR_FLAGS} \
    "${WASM}"

echo "==> build"
cmake --build "${BUILD}"

echo "==> install to dist/"
cmake --install "${BUILD}"

echo "Done. Output in ${WASM}/dist/"
