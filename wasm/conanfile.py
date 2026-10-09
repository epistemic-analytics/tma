from conan import ConanFile
from conan.tools.cmake import CMakeToolchain, CMakeDeps


class TmaWasmConan(ConanFile):
    name     = "tma-wasm"
    settings = "os", "arch", "compiler", "build_type"

    def requirements(self):
        # Armadillo only (the version libqe-wasm builds with).  The libtma and
        # libqe headers are vendored into include/ by scripts/sync-headers.sh,
        # not taken as Conan requirements: libqe's recipe pins an older
        # Armadillo, which would conflict with this one.
        self.requires("armadillo/15.2.6")

    def generate(self):
        # Propagate ARMA_DONT_USE_BLAS/LAPACK to the CMake build so that
        # Armadillo never emits BLAS/LAPACK symbol references that Emscripten
        # cannot resolve.
        tc = CMakeToolchain(self)
        tc.preprocessor_definitions["ARMA_DONT_USE_BLAS"]   = "1"
        tc.preprocessor_definitions["ARMA_DONT_USE_LAPACK"] = "1"
        tc.generate()
        CMakeDeps(self).generate()
