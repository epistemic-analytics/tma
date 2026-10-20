from conan import ConanFile
from conan.tools.files import copy
from conan.tools.layout import basic_layout
import os


class LibtmaConan(ConanFile):
    name         = "libtma"
    version      = "0.0.0"   # placeholder — set_version() always overrides this
    description  = ("Header-only C++ accumulation layer of tma (stanza windows, "
                    "context tensors, weight models) built on libqe; shared by tma, "
                    "qe-tma, tma-wasm and TMA.jl")
    license      = "GPL-3.0-only"
    url          = "https://gitlab.com/epistemic-analytics/qe-packages/tma"
    homepage     = url
    topics       = ("header-only", "quantitative-ethnography", "tma", "armadillo")
    package_type = "header-library"

    # The canonical headers live in the R package's inst/include/libtma (this
    # recipe sits in conan/), so export them from there, bind/ helpers included.
    def export_sources(self):
        copy(self, "*.hpp",
             src=os.path.join(self.recipe_folder, "..", "inst", "include", "libtma"),
             dst=os.path.join(self.export_sources_folder, "include", "libtma"))

    no_copy_source = True

    # ── version: single source of truth is tma's DESCRIPTION ───────────────
    # cranqe overrides it with --version (the conan/vX.Y.Z tag on a release,
    # X.Y.Z-dev.N on a dev build).
    def set_version(self):
        desc = os.path.join(self.recipe_folder, "..", "DESCRIPTION")
        with open(desc) as f:
            for line in f:
                if line.startswith("Version:"):
                    self.version = line.split(":", 1)[1].strip()
                    return

    # ── dependencies ─────────────────────────────────────────────────────────
    def requirements(self):
        # libqe's generic headers (adjacency, bind/), and
        # through it Armadillo.  Same floor as DESCRIPTION's LinkingTo.
        # libqe < 0.2.0 also ships a copy of libtma; this package's include
        # directory comes first for consumers.
        self.requires("libqe/[>=0.2.1 <0.3]", transitive_headers=True)

    def layout(self):
        basic_layout(self, src_folder=".")

    def package_id(self):
        # Header-only: one package for every configuration.
        self.info.clear()

    def package(self):
        copy(self, "*.hpp",
             src=os.path.join(self.source_folder, "include"),
             dst=os.path.join(self.package_folder, "include"))

    def package_info(self):
        self.cpp_info.bindirs = []
        self.cpp_info.libdirs = []
        # find_package(libtma) / target_link_libraries(... libtma::libtma)
        self.cpp_info.set_property("cmake_file_name",   "libtma")
        self.cpp_info.set_property("cmake_target_name", "libtma::libtma")
