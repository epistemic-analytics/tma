cmake_minimum_required(VERSION 3.18)
project(test_libtma CXX)

find_package(libtma    REQUIRED)
find_package(Armadillo REQUIRED)

add_executable(test_libtma src/test_libtma.cpp)
target_compile_features(test_libtma PRIVATE cxx_std_17)
# Use libqe's built-in linear algebra (linalg_fallback.hpp), not LAPACK: the
# test only checks that the headers compile, link and run, and ConanCenter's
# prebuilt OpenBLAS binary died with SIGILL ("Illegal instruction", exit 132)
# on a cranqe runner whose CPU lacks the instructions it was built for -- as
# rENA's libena test package did (rENA eff9fa8).
target_compile_definitions(test_libtma PRIVATE ARMA_DONT_USE_BLAS ARMA_DONT_USE_LAPACK)
target_link_libraries(test_libtma PRIVATE libtma::libtma ${ARMADILLO_LIBRARIES})
target_include_directories(test_libtma PRIVATE ${ARMADILLO_INCLUDE_DIRS})
