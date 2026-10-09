cmake_minimum_required(VERSION 3.18)
project(libtma_julia LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# ── Julia + CxxWrap (as LibQE.jl's CMakeLists.txt) ────────────────────────────
# find_package(Julia) needs FindJulia.cmake, which lives inside the JlCxx
# artifact, so locate the CxxWrap prefix first via julia itself.
find_program(Julia_EXECUTABLE julia REQUIRED)

execute_process(
    COMMAND "${Julia_EXECUTABLE}" --project=@. --startup-file=no -e
            "using CxxWrap; print(CxxWrap.prefix_path())"
    WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
    OUTPUT_VARIABLE JLCXX_PREFIX
    OUTPUT_STRIP_TRAILING_WHITESPACE
    RESULT_VARIABLE JLCXX_RESULT
)
if(NOT JLCXX_RESULT EQUAL 0)
    message(FATAL_ERROR
        "Could not locate CxxWrap prefix. "
        "Install it in Julia with: using Pkg; Pkg.add(\"CxxWrap\")")
endif()

list(PREPEND CMAKE_MODULE_PATH "${JLCXX_PREFIX}/lib/cmake/JlCxx")
list(PREPEND CMAKE_PREFIX_PATH "${JLCXX_PREFIX}")

find_package(Julia REQUIRED)
find_package(JlCxx REQUIRED)

# ── Armadillo ─────────────────────────────────────────────────────────────────
find_package(Armadillo REQUIRED)

# ── libtma + libqe headers ────────────────────────────────────────────────────
# In a tma checkout, scripts/sync-headers.sh vendors both into julia/include/
# (libtma from inst/include, libqe from Conan).  A registered TMA.jl install
# contains julia/ only, so without that copy fall back to Conan packages
# (`conan install --requires=libtma/<version> -r qe-libs`, then pass the
# generated conan_toolchain.cmake): libtma's brings libqe's along.
set(HEADER_DIR "${CMAKE_CURRENT_SOURCE_DIR}/include")
if(EXISTS "${HEADER_DIR}/libtma/libtma.hpp"
   AND EXISTS "${HEADER_DIR}/libqe/bind/cxxwrap.hpp")
    set(LIBTMA_TARGET "")
    message(STATUS "libtma / libqe: using headers in ${HEADER_DIR}")
else()
    find_package(libtma REQUIRED)
    set(HEADER_DIR "")
    set(LIBTMA_TARGET libtma::libtma)
    message(STATUS "libtma / libqe: using Conan-provided packages")
endif()

# ── shared library ────────────────────────────────────────────────────────────
add_library(libtma_julia SHARED csrc/libtma_julia.cpp)

target_include_directories(libtma_julia PRIVATE
    ${HEADER_DIR}
    ${ARMADILLO_INCLUDE_DIRS}
)

# cxxwrap_julia_stl provides the std::vector wrappers (jlcxx/stl.hpp) used for
# arguments and the result tuples; macOS's linker rejects its symbols being
# left unresolved.
target_link_libraries(libtma_julia PRIVATE
    JlCxx::cxxwrap_julia
    JlCxx::cxxwrap_julia_stl
    ${LIBTMA_TARGET}
    ${ARMADILLO_LIBRARIES}
)

# On macOS, Accelerate provides BLAS/LAPACK; on Linux use the system LAPACK.
if(APPLE)
    target_link_libraries(libtma_julia PRIVATE "-framework Accelerate")
else()
    find_package(LAPACK REQUIRED)
    find_package(BLAS   REQUIRED)
    target_link_libraries(libtma_julia PRIVATE ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
endif()

# CxxWrap requires the library name to match what @wrapmodule expects.
set_target_properties(libtma_julia PROPERTIES
    PREFIX ""
    OUTPUT_NAME "libtma_julia"
)

install(TARGETS libtma_julia
    LIBRARY DESTINATION "${CMAKE_CURRENT_SOURCE_DIR}/lib"
)
