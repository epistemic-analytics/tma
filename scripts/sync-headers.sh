#!/bin/sh
# sync-headers.sh — vendor the C++ headers that qe-tma's extension (python/),
#                   tma-wasm's module (wasm/) and TMA.jl's library (julia/)
#                   build against into python/include/, wasm/include/ and
#                   julia/include/:
#
#   libtma  from inst/include/libtma/ (this repo's canonical copy)
#   libqe   from Conan: libqe/$LIBQE_VERSION on the qe-libs registry, or from a
#           local libqe checkout when LIBQE_INCLUDE=<libqe>/include is set
#
# The R package doesn't need this: it gets libqe through LinkingTo.  Needed for:
#
#   1. Building or installing qe-tma from a checkout:
#        sh scripts/sync-headers.sh && pip install ./python
#   2. A self-contained qe-tma sdist (it ships python/include/):
#        sh scripts/sync-headers.sh && python -m build --sdist python/
#      cranqe runs this script before building, when the repo provides it.
#   3. tma-wasm: wasm/scripts/build.sh runs it.
#   4. TMA.jl from a checkout (cranqe's Julia build runs it too).
#
# Conan: uses `conan` on PATH, else installs it into a throwaway venv with
# $PYTHON (default python3).  It runs with a throwaway CONAN_HOME, so your own
# Conan configuration and cache are untouched.
#
# Usage (from anywhere):  sh scripts/sync-headers.sh

set -e
REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
# Bump with DESCRIPTION's LinkingTo: libqe minimum.
LIBQE_VERSION="${LIBQE_VERSION:-0.1.9}"
CONAN_REMOTE="https://gitlab.com/api/v4/projects/22522458/packages/conan"
DESTS="${REPO_ROOT}/python/include ${REPO_ROOT}/wasm/include ${REPO_ROOT}/julia/include"

TMP="$(mktemp -d)"
trap 'rm -rf "${TMP}"' EXIT

# ── libqe headers ─────────────────────────────────────────────────────────────
if [ -n "${LIBQE_INCLUDE:-}" ]; then
    LIBQE_SRC="${LIBQE_INCLUDE}/libqe"
    LIBQE_FROM="${LIBQE_SRC}"
else
    if command -v conan >/dev/null 2>&1; then
        CONAN=conan
    else
        echo "Installing conan into a throwaway venv (${PYTHON:-python3})..."
        "${PYTHON:-python3}" -m venv "${TMP}/venv"
        VBIN="${TMP}/venv/bin"
        [ -d "${VBIN}" ] || VBIN="${TMP}/venv/Scripts"   # Windows venv layout
        "${VBIN}/python" -m pip install --quiet conan
        CONAN="${VBIN}/conan"
    fi

    export CONAN_HOME="${TMP}/conan-home"
    ${CONAN} remote add qe-libs "${CONAN_REMOTE}" >/dev/null
    echo "Downloading libqe/${LIBQE_VERSION} from ${CONAN_REMOTE}..."
    ${CONAN} download "libqe/${LIBQE_VERSION}:*" -r qe-libs >/dev/null
    # libqe is header-only: one package, the only one in this throwaway cache.
    LIBQE_SRC="$(find "${CONAN_HOME}/p" -type d -path "*/p/include/libqe" | head -1)"
    if [ -z "${LIBQE_SRC}" ]; then
        echo "ERROR: libqe/${LIBQE_VERSION} package has no include/libqe" >&2
        exit 1
    fi
    LIBQE_FROM="libqe/${LIBQE_VERSION} (Conan)"
fi

# ── copy into each destination ───────────────────────────────────────────────
for DST in ${DESTS}; do
    # Generated (git-ignored) copies: clear them so a header that moved or was
    # removed upstream is not left behind.
    rm -rf "${DST}/libtma" "${DST}/libqe"
    mkdir -p "${DST}"
    echo "Syncing inst/include/libtma -> ${DST}/libtma"
    cp -R "${REPO_ROOT}/inst/include/libtma" "${DST}/libtma"
    echo "Syncing ${LIBQE_FROM} -> ${DST}/libqe"
    cp -R "${LIBQE_SRC}" "${DST}/libqe"
done
echo "Done."
