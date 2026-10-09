# TMA.jl

Julia bindings for libtma, the accumulation C++ layer of
[tma](https://gitlab.com/epistemic-analytics/qe-packages/tma) (Transmodal
Analysis), shared with the R package, qe-tma (Python) and tma-wasm: stanza
windows, ground/response and context-tensor accumulation, row connections and
weight models. The C++ lives in this repository (`inst/include/libtma`) and
builds on [libqe](https://github.com/epistemic-analytics/libqe)'s generic
numerics.

These functions moved here from LibQE.jl, which keeps them only until libqe
0.2.0. Built with [CxxWrap.jl](https://github.com/JuliaInterop/CxxWrap.jl);
results are plain NamedTuples, so TMA.jl loads alongside LibQE.jl and ENA.jl.

## Install

TMA.jl is in the QE package registry:

```julia
using Pkg
Pkg.Registry.add(RegistrySpec(url = "https://gitlab.com/epistemic-analytics/qe-packages/cranqe.git#julia"))
Pkg.add("TMA")
```

Its C++ library (`lib/libtma_julia`) is built with CMake; it needs a C++17
compiler, CMake, [Armadillo](https://arma.sourceforge.net/) and CxxWrap 0.15
(Julia 1.11; CxxWrap 0.15 has no Julia 1.13 binary).

From a tma checkout, vendor the headers first:

```sh
sh scripts/sync-headers.sh            # repo root: libtma + libqe (from Conan) → julia/include
cd julia
julia --project=. -e 'using Pkg; Pkg.instantiate()'
cmake -B build -DCMAKE_BUILD_TYPE=Release .
cmake --build build && cmake --install build      # → julia/lib/libtma_julia
julia --project=. -e 'using Pkg; Pkg.test()'
```

Outside a checkout (a registered install contains `julia/` only), CMake takes
libtma and libqe from their Conan packages instead:

```sh
conan remote add qe-libs https://gitlab.com/api/v4/projects/22522458/packages/conan
conan install --requires=libtma/<version> -r qe-libs -g CMakeDeps -g CMakeToolchain \
      --output-folder=build/conan
cmake -B build -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_TOOLCHAIN_FILE=build/conan/conan_toolchain.cmake .
```

## Quick start

```julia
using TMA

codes = Float64[2 0 1; 1 3 0; 0 1 2]          # 3 rows × 3 codes

accumulate_stanza(codes; window_back = 2)      # 3 × choose(3, 2) co-occurrences

# One unit, default tensor (weight 1, window 3); 0-based response rows
r = accumulate_tensor_unit(codes, [1.0, 3.0], Int32[2], Int32[], Int32[], Int32[],
                           zeros(Int32, 3, 0), Int32[0, 1, 2], [0.0, 1.0, 2.0])
aggregate_row_connections(r.row_connection_counts, 3; weight = "sqrt")
```

## API

| Function | Returns |
|----------|---------|
| `connection_matrix(ground, response; response_weight=1.0, ordered=true)` | `n × n` for one ground/response pair |
| `accumulate_stanza(codes; window_back=1, window_forward=0, binary=true, ordered=false)` | `n_rows × choose(n,2)` (or `n²` ordered) |
| `row_connections(codes; binary=true)` | per-row co-occurrence |
| `rolling_window_sum(codes; window_size=1)` | backward window sum |
| `flat_index(indices, dims)` | 0-based column-major linear index |
| `accumulate_unit(codes, unit_rows, decay_fn; ordered=false)` | ground/response unit vector |
| `accumulate_unit_with_rows(codes, unit_rows, decay_fn; ordered=false)` | `(networks, row_networks)` |
| `accumulate_tensor_unit(codes, tensor, dims, …, unit_rows, times; ordered=true)` | `(connection_counts, row_connection_counts)` |
| `aggregate_row_connections(row_conn, n_codes; ordered=false, weight="binary")` | unit vector, weight model per row |
| `finalize_row_connections(row_conn, n_codes; ordered=false, weight="binary")` | the per-row step of the above |

`weight` is `"binary" | "product" | "sqrt" | "log1p"` or a legacy `Bool`.
Docstrings (`?accumulate_tensor_unit`) give the full argument lists.
