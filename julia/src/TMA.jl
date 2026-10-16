"""
    TMA

Julia bindings for libtma — the accumulation C++ layer of
[tma](https://gitlab.com/epistemic-analytics/qe-packages/tma) (Transmodal
Analysis), shared with the R package, qe-tma (Python) and tma-wasm: stanza
windows, ground/response and context-tensor accumulation, row connections and
weight models.

The C++ lives in the tma repository (`inst/include/libtma`) and builds on
libqe's generic numerics.  These functions moved here from LibQE.jl (libqe's
phase 4b split) with the same signatures and results.

All functions accept standard Julia `Matrix{Float64}` / `Vector{Float64}`
arguments.  Matrix inputs are zero-copy (Julia and Armadillo are both
column-major); results are copied out once.

## Build

Build the C++ shared library before loading this package (see README.md):

```sh
sh ../scripts/sync-headers.sh      # from julia/ in a tma checkout
cmake -B build -DCMAKE_BUILD_TYPE=Release .
cmake --build build
cmake --install build              # copies libtma_julia → julia/lib/
```
"""
# CxxWrap loads a native shared library at module init time; precompilation
# of the method table is not safe across different library builds.
__precompile__(false)

module TMA

using CxxWrap

# ── Public API ────────────────────────────────────────────────────────────────
export
    connection_matrix, accumulate_stanza, row_connections,
    rolling_window_sum, flat_index, accumulate_unit,
    accumulate_unit_with_rows, accumulate_tensor_unit,
    aggregate_row_connections, finalize_row_connections,
    aggregate_self_connections, finalize_self_connections

# ── Load shared library ───────────────────────────────────────────────────────
# The library built by CMake is installed into julia/lib/ (one level up from
# this file's julia/src/ directory).  cranqe runs the build step before
# invoking Pkg.test(); for local testing build it manually first (see above).
const _lib_dir  = joinpath(@__DIR__, "..", "lib")
const _lib_name = "libtma_julia"

function _lib_path()
    for ext in ("", ".so", ".dylib", ".dll")
        p = joinpath(_lib_dir, _lib_name * ext)
        isfile(p) && return p
    end
    error("libtma_julia shared library not found in $(_lib_dir). " *
          "Build it first: cd julia && cmake -B build -DCMAKE_BUILD_TYPE=Release . " *
          "&& cmake --build build && cmake --install build")
end

@wrapmodule(_lib_path)

function __init__()
    @initcxx
end

# ── StdVector converters ──────────────────────────────────────────────────────
# CxxWrap 0.15 exposes C++ std::vector<T> params as StdVector{T}, not Vector{T}.
# Julia does not auto-convert between the two, so we do it explicitly.

function _sv_i32(v::AbstractVector{<:Integer})
    sv = CxxWrap.StdLib.StdVector{Int32}()
    for x in v; push!(sv, Int32(x)); end
    sv
end

# Matrices come back from C++ as (column-major data, rows, cols).  StdVector
# does not implement AbstractArray, so collect() before reshape.
_mat(data, rows, cols) = reshape(collect(Float64, data), Int(rows), Int(cols))

# ── Accumulation ──────────────────────────────────────────────────────────────

"""
    connection_matrix(ground, response; response_weight=1.0, ordered=true)
    -> Matrix{Float64}

Core adjacency math for one ground+response event pair.
"""
function connection_matrix(ground::Vector{Float64}, response::Vector{Float64};
                                     response_weight::Float64 = 1.0, ordered::Bool = true)
    n = length(ground)
    reshape(connection_matrix(ground, Int32(n), response, Int32(n),
                                           response_weight, ordered), n, n)
end

"""
    accumulate_stanza(codes; window_back=1, window_forward=0, binary=true, ordered=false)
    -> Matrix{Float64}

Stanza-window accumulation for one conversation.

- `ordered=false` (default): undirected upper-tri co-occurrences.
  Returns `n_rows × choose(n_codes, 2)`.
  Pass `typemax(Int32)` for an infinite back window.
- `ordered=true`: directed — focal row as response, prior window rows as
  ground. Returns `n_rows × n_codes²`. `window_forward` is ignored.
"""
function accumulate_stanza(codes::Matrix{Float64};
                        window_back::Integer    = 1,
                        window_forward::Integer = 0,
                        binary::Bool            = true,
                        ordered::Bool           = false)
    rows, cols = size(codes)
    n_out = ordered ? cols * cols : cols * (cols - 1) ÷ 2
    result = accumulate_stanza(vec(codes), Int32(rows), Int32(cols),
                               Int32(window_back), Int32(window_forward),
                               binary, ordered)
    reshape(result, rows, n_out)
end

"""
    row_connections(codes; binary=true) -> Matrix{Float64}

Per-row upper-triangle co-occurrence matrix without windowing.
"""
function row_connections(codes::Matrix{Float64}; binary::Bool = true)
    rows, cols = size(codes)
    n_tri = cols * (cols - 1) ÷ 2
    reshape(row_connections(vec(codes), Int32(rows), Int32(cols), binary),
            rows, n_tri)
end

"""
    rolling_window_sum(codes; window_size=1) -> Matrix{Float64}

Rolling backward sum of raw code values (no upper-tri transform).
"""
function rolling_window_sum(codes::Matrix{Float64}; window_size::Integer = 1)
    rows, cols = size(codes)
    reshape(rolling_window_sum(vec(codes), Int32(rows), Int32(cols), Int32(window_size)),
            rows, cols)
end

"""
    flat_index(indices, dims) -> Int

Column-major linear index into a multi-dimensional array. `indices` and `dims`
are **0-based** integer vectors.
"""
function flat_index(indices::AbstractVector{<:Integer}, dims::AbstractVector{<:Integer})
    # CxxWrap 0.15 exposes flat_index as (StdVector{Int32}, StdVector{Int32}).
    # Vector{Int32} does not auto-convert, so we build StdVectors explicitly to
    # break the dispatch loop that would occur from calling flat_index(Int32.(v),...).
    sv_idx = CxxWrap.StdLib.StdVector{Int32}()
    sv_dim = CxxWrap.StdLib.StdVector{Int32}()
    for x in indices; push!(sv_idx, Int32(x)); end
    for x in dims;    push!(sv_dim, Int32(x)); end
    flat_index(sv_idx, sv_dim)
end

"""
    accumulate_unit(codes, unit_rows, decay_fn; ordered=false) -> Vector{Float64}

Ground/response accumulation for one unit (tma model).
`unit_rows` is a **0-based** `Vector{Int32}`.
`decay_fn(distances::Vector{Float64}) -> Vector{Float64}` maps distances to weights.
"""
function accumulate_unit(codes::Matrix{Float64}, unit_rows::AbstractVector{<:Integer},
                          decay_fn::Function; ordered::Bool = false)
    rows, cols = size(codes)
    accumulate_unit(vec(codes), Int32(rows), Int32(cols),
                    _sv_i32(unit_rows), decay_fn, ordered)
end

"""
    accumulate_unit_with_rows(codes, unit_rows, decay_fn; ordered=false)
    -> NamedTuple{(:networks, :row_networks)}

Like `accumulate_unit` but also returns the per-response-row connection matrix.
"""
function accumulate_unit_with_rows(codes::Matrix{Float64}, unit_rows::AbstractVector{<:Integer},
                                    decay_fn::Function; ordered::Bool = false)
    rows, cols = size(codes)
    t = accumulate_unit_with_rows(vec(codes), Int32(rows), Int32(cols),
                                  _sv_i32(unit_rows), decay_fn, ordered)
    (
        networks     = collect(Float64, t[1]),
        row_networks = _mat(t[2], t[3], t[4]),
    )
end

"""
    accumulate_tensor_unit(codes, dims, dims_sender, dims_receiver, dims_mode,
                           context_lookup, unit_rows, times; ordered=true, misses=false)
    -> NamedTuple{(:connection_counts, :row_connection_counts)}

tma tensor-based accumulation for one unit.

- `codes`          — `Matrix{Float64}` (n_context_rows × n_codes)
- `tensor`         — `Vector{Float64}` flat column-major, shaped by `dims`
- `dims`           — `Vector{Int32}` tensor axis sizes
- `dims_sender`    — `Vector{Int32}` axis indices for sender factors
- `dims_receiver`  — `Vector{Int32}` axis indices for receiver factors
- `dims_mode`      — `Vector{Int32}` axis indices for mode factors
- `context_lookup` — `Matrix{Int32}` (n_context_rows × n_factors), **0-based**
- `unit_rows`      — `Vector{Int32}` **0-based** response-row indices for this unit
- `times`          — `Vector{Float64}` one timestamp per context row
- `ordered`        — `true` → directed n²; `false` → undirected upper-tri
- `misses`         — `true` → also return missed co-occurrences: ground code i
                     present while response code j is absent (directed n² rows,
                     no response self-term)

Returns `(connection_counts, row_connection_counts)`; with `misses = true`,
`(connection_counts, row_connection_counts, miss_counts, row_miss_counts)`.
"""
function accumulate_tensor_unit(codes::Matrix{Float64},
                                 tensor::Vector{Float64},
                                 dims::AbstractVector{<:Integer},
                                 dims_sender::AbstractVector{<:Integer},
                                 dims_receiver::AbstractVector{<:Integer},
                                 dims_mode::AbstractVector{<:Integer},
                                 context_lookup::Matrix{<:Integer},
                                 unit_rows::AbstractVector{<:Integer},
                                 times::Vector{Float64};
                                 ordered::Bool = true,
                                 misses::Bool = false)
    rows, cols = size(codes)
    cl_rows, cl_cols = size(context_lookup)
    r = (misses ? apply_tensor_unit_misses : apply_tensor_unit)(
            tensor,
            _sv_i32(dims), _sv_i32(dims_sender),
            _sv_i32(dims_receiver), _sv_i32(dims_mode),
            vec(Int32.(context_lookup)), Int32(cl_rows), Int32(cl_cols),
            _sv_i32(unit_rows),
            vec(codes), Int32(rows), Int32(cols),
            times,
            ordered)
    hits = (
        connection_counts     = collect(Float64, r[1]),
        row_connection_counts = _mat(r[2], r[3], r[4]),
    )
    misses || return hits
    (
        hits...,
        miss_counts     = collect(Float64, r[5]),
        row_miss_counts = _mat(r[6], r[3], r[4]),
    )
end

# ── Weight models (not bound by LibQE.jl; added with the move) ────────────────

# A weight-model name, or the legacy Bool binary flag (true → "binary",
# false → "product"), as in R and Python.
_weight_name(w::AbstractString) = String(w)
_weight_name(w::Bool)           = w ? "binary" : "product"

"""
    aggregate_row_connections(row_conn, n_codes; ordered=false, weight="binary")
    -> Vector{Float64}

Fold `accumulate_tensor_unit`'s `row_connection_counts` (n_response_rows ×
n_codes², per-row directed counts) into a unit vector, applying the weight
model to each row before the sum (rENA's `weight.by`).

`weight`: `"binary"` (unordered: clamp to 1; ordered: raw counts), `"product"`
(raw counts), `"sqrt"`, `"log1p"` (alias `"log"`), or a legacy `Bool`.
Returns length `n_codes²` (ordered) or `choose(n_codes, 2)` (unordered).
"""
function aggregate_row_connections(row_conn::Matrix{Float64}, n_codes::Integer;
                                   ordered::Bool = false,
                                   weight::Union{AbstractString,Bool} = "binary")
    rows, cols = size(row_conn)
    collect(Float64, aggregate_row_connections(vec(row_conn), Int32(rows), Int32(cols),
                                               Int32(n_codes), ordered, _weight_name(weight)))
end

"""
    finalize_row_connections(row_conn, n_codes; ordered=false, weight="binary")
    -> Matrix{Float64}

The per-row step of `aggregate_row_connections` (fold + weight model) without
the sum: its rows sum to `aggregate_row_connections`.  Returns
`n_response_rows × n_codes²` (ordered) or `× choose(n_codes, 2)`.
"""
function finalize_row_connections(row_conn::Matrix{Float64}, n_codes::Integer;
                                  ordered::Bool = false,
                                  weight::Union{AbstractString,Bool} = "binary")
    rows, cols = size(row_conn)
    n_out = ordered ? n_codes^2 : n_codes * (n_codes - 1) ÷ 2
    reshape(collect(Float64, finalize_row_connections(vec(row_conn), Int32(rows), Int32(cols),
                                                      Int32(n_codes), ordered,
                                                      _weight_name(weight))),
            rows, n_out)
end

"""
    finalize_self_connections(row_conn, n_codes; ordered=false, weight="binary")
    -> Matrix{Float64}

Per-line self-connections: the diagonal (ground i × response i) of each
directed row of `row_connection_counts` (or `row_miss_counts`, for
self-misses), weighted per line as `finalize_row_connections` weights it.
Returns `n_response_rows × n_codes`.
"""
function finalize_self_connections(row_conn::Matrix{Float64}, n_codes::Integer;
                                   ordered::Bool = false,
                                   weight::Union{AbstractString,Bool} = "binary")
    rows, cols = size(row_conn)
    reshape(collect(Float64, finalize_self_connections(vec(row_conn), Int32(rows), Int32(cols),
                                                       Int32(n_codes), ordered,
                                                       _weight_name(weight))),
            rows, n_codes)
end

"""
    aggregate_self_connections(row_conn, n_codes; ordered=false, weight="binary")
    -> Vector{Float64}

`finalize_self_connections` summed over the lines (length `n_codes`).
"""
function aggregate_self_connections(row_conn::Matrix{Float64}, n_codes::Integer;
                                    ordered::Bool = false,
                                    weight::Union{AbstractString,Bool} = "binary")
    rows, cols = size(row_conn)
    collect(Float64, aggregate_self_connections(vec(row_conn), Int32(rows), Int32(cols),
                                                Int32(n_codes), ordered, _weight_name(weight)))
end

end # module TMA
