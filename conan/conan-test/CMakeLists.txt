cmake_minimum_required(VERSION 3.18)
project(test_libtma CXX)

find_package(libtma    REQUIRED)
find_package(Armadillo REQUIRED)

add_executable(test_libtma src/test_libtma.cpp)
target_compile_features(test_libtma PRIVATE cxx_std_17)
target_link_libraries(test_libtma PRIVATE libtma::libtma ${ARMADILLO_LIBRARIES})
target_include_directories(test_libtma PRIVATE ${ARMADILLO_INCLUDE_DIRS})
