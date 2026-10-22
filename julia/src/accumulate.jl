# High-level accumulation: contexts, context_tensor and accumulate, as R tma's
# contexts() / context_tensor() / accumulate() and qe-tma's (the Python
# binding) — the same rules, the same per-context libtma call
# (accumulate_unit_networks), the same results.
#
#   data  = (u = [...], c = [...], A = [...], B = [...])     # any Tables.jl table
#   accum = TMA.accumulate(data, :u, :c, [:A, :B];
#                          tensor = context_tensor(data; default_window = 4),
#                          split_by = :c)
#   accum.connection_counts      # units × connections (= R connection.counts)
#
# `accumulate` is not exported (Base has one): call it TMA.accumulate.

# ── Context tensor ────────────────────────────────────────────────────────────

"""
    ContextTensor

Per-sender / receiver / mode weights and windows, `array` of size
`(n_s1[, …], n_r1[, …][, n_modes], 2)`; the last axis is `[weight, window]`.
Set entries in `array` to change them. Built by [`context_tensor`](@ref).
"""
struct ContextTensor
    array::Array{Float64}
    sender_cols::Vector{Symbol}
    receiver_cols::Vector{Symbol}
    mode_col::Union{Symbol,Nothing}
    sender_levels::Vector{Vector{Any}}
    receiver_levels::Vector{Vector{Any}}
    mode_levels::Vector{Any}
end

# Kernel arguments: shape, and 0-based axis indices of each factor kind.
_tensor_dims(t::ContextTensor) = Int32[size(t.array)...]
function _tensor_inds(t::ContextTensor)
    ns, nr = length(t.sender_cols), length(t.receiver_cols)
    (Int32.(0:ns-1), Int32.(ns:ns+nr-1), t.mode_col === nothing ? Int32[] : Int32[ns+nr])
end

_symbols(x) = x === nothing ? Symbol[] : x isa Union{Symbol,AbstractString} ? [Symbol(x)] : Symbol.(collect(x))

"""
    context_tensor(data; sender_cols=nothing, receiver_cols=nothing, mode_col=nothing,
                   default_window=1, default_weight=1) -> ContextTensor

A tensor of weights and windows over the levels of the given columns (= R
tma's `context_tensor()`): every entry starts at `default_weight` /
`default_window`. Without sender, receiver or mode columns it is the default
tensor (one weight, one window for every line).
"""
function context_tensor(data; sender_cols = nothing, receiver_cols = nothing, mode_col = nothing,
                        default_window::Real = 1.0, default_weight::Real = 1.0)
    cols = Tables.columntable(data)
    s, r = _symbols(sender_cols), _symbols(receiver_cols)
    m = mode_col === nothing ? nothing : Symbol(mode_col)
    lev(c) = Any[sort(unique(cols[c]))...]
    slev, rlev = [lev(c) for c in s], [lev(c) for c in r]
    mlev = m === nothing ? Any[] : lev(m)
    shape = (length.(slev)..., length.(rlev)..., (m === nothing ? () : (length(mlev),))..., 2)
    arr = Array{Float64}(undef, shape)
    selectdim(arr, ndims(arr), 1) .= default_weight
    selectdim(arr, ndims(arr), 2) .= default_window
    ContextTensor(arr, s, r, m, slev, rlev, mlev)
end

# 0-based tensor indices of every line (n_rows × n_factors) for the kernel.
function _context_lookup(t::ContextTensor, cols, n::Integer)
    facs = [(t.sender_cols[i], t.sender_levels[i]) for i in eachindex(t.sender_cols)]
    append!(facs, [(t.receiver_cols[i], t.receiver_levels[i]) for i in eachindex(t.receiver_cols)])
    t.mode_col === nothing || push!(facs, (t.mode_col, t.mode_levels))
    look = zeros(Int32, n, length(facs))
    for (j, (c, levels)) in enumerate(facs), i in 1:n
        k = findfirst(isequal(cols[c][i]), levels)
        k === nothing && error("value $(repr(cols[c][i])) of $(c) is not a level of the tensor; " *
                               "build the tensor from the same data")
        look[i, j] = k - 1
    end
    look
end

# ── Contexts ──────────────────────────────────────────────────────────────────

"""
    rules(fs...) -> Vector

Bundle head-of-others (HOO) rules. Each rule is `f(data, unit) -> AbstractVector{Bool}`:
`data` is the table's columns (a `NamedTuple` of vectors), `unit` one of the
unit's lines (a `NamedTuple`, R's `UNIT`). A line is visible to a unit when any
rule marks it.
"""
rules(fs...) = Any[fs...]

"""
    conversation_rules(cols...) -> Vector

The rule "same values as the unit in `cols`" (= R's
`conversation_rules(A == UNIT\$A & B == UNIT\$B)`).
"""
function conversation_rules(cols...)
    isempty(cols) && throw(ArgumentError("conversation_rules() needs at least one column"))
    cs = Symbol.(collect(cols))
    Any[(data, unit) -> reduce((m, c) -> m .& isequal.(data[c], unit[c]), cs;
                               init = trues(length(data[first(cs)])))]
end

"""
    ContextModel

Each unit's own lines and the lines it can see (1-based row indices into the
data), and its contexts split by `split_by` (one context when not split).
"""
struct ContextModel
    unit_labels::Vector{String}
    unit_own_rows::Dict{String,Vector{Int}}
    contexts_map::Dict{String,Vector{Int}}
    splits_map::Dict{String,Vector{Vector{Int}}}
end

"""
    contexts(data; units, conversations=nothing, hoo_rules=nothing,
             include_self=true, split_by=nothing) -> ContextModel

Each unit's context (= R tma's `contexts()`): the lines a unit can see, by
`conversations` (shorthand for `conversation_rules(conversations...)`) and/or
`hoo_rules` (union). `split_by` (= R's `split_rules = <columns>`) splits each
context by those columns' values, in order of first appearance; each split is
accumulated on its own (windows never cross one). Units are labelled by their
`units` values joined with `"::"`, in order of first appearance.
"""
function contexts(data; units, conversations = nothing, hoo_rules = nothing,
                  include_self::Bool = true, split_by = nothing)
    conversations === nothing && hoo_rules === nothing &&
        throw(ArgumentError("give conversations= or hoo_rules="))
    cols = Tables.columntable(data)
    n = length(first(cols))
    ucols = _symbols(units)
    active = Any[]
    conversations === nothing || append!(active, conversation_rules(_symbols(conversations)...))
    hoo_rules === nothing || append!(active, hoo_rules)
    for c in [ucols; _symbols(split_by)]
        haskey(cols, c) || throw(KeyError(c))
    end

    keys = [join((string(cols[c][i]) for c in ucols), "::") for i in 1:n]
    labels = unique(keys)
    rowof(i) = NamedTuple{propertynames(cols)}(Tuple(cols[c][i] for c in propertynames(cols)))
    own_rows = Dict(l => Int[] for l in labels)
    for (i, k) in enumerate(keys)
        push!(own_rows[k], i)
    end
    ctx = Dict{String,Vector{Int}}()
    for l in labels
        visible = falses(n)
        for i in own_rows[l], rule in active      # every line the unit occupies
            visible .|= rule(cols, rowof(i))
        end
        include_self ? (visible[own_rows[l]] .= true) : (visible[own_rows[l]] .= false)
        ctx[l] = findall(visible)
    end
    scols = _symbols(split_by)
    splits = Dict{String,Vector{Vector{Int}}}()
    for l in labels
        if isempty(scols)
            splits[l] = [ctx[l]]
        else
            groups = Dict{Any,Vector{Int}}(); order = Any[]
            for r in ctx[l]
                k = Tuple(string(cols[c][r]) for c in scols)
                haskey(groups, k) || (push!(order, k); groups[k] = Int[])
                push!(groups[k], r)
            end
            splits[l] = [groups[k] for k in order]
        end
    end
    ContextModel(labels, own_rows, ctx, splits)
end

# ── Accumulate ────────────────────────────────────────────────────────────────

"""
    TMAAccumulation

Each unit's networks, not modeled (= R tma's `accumulate()` result; the
fields of qe-tma's `TMAAccumulation`, without trailing underscores):

- `connection_counts` — units × connections, weighted and summed per unit.
  Columns follow `adjacency_key`: ordered `p²` (ground i → response j at
  `(j-1)p+i`); unordered `choose(p, 2)`, or `p(p+1)/2` with `self_connections`
  (A & A, A & B, B & B, A & C, …).
- `self_connection_counts` — units × codes: ground i × response i.
- `miss_counts`, `self_miss_counts` — missed co-occurrences, or `nothing`
  with `misses = false`.
- `row_connection_counts`, `row_self_connection_counts`, `row_miss_counts`,
  `row_self_miss_counts` — the same per data line, raw (zero for lines that
  are no unit's own).
- `unit_labels`, `codes`, `connection_names` ("i & j"), `adjacency_key`
  (`[(code_i, code_j), …]`), `meta` (one row per unit: the first line's
  non-code columns, a column table), `ordered`, `self_connections`, `weight_by`.
"""
struct TMAAccumulation
    connection_counts::Matrix{Float64}
    self_connection_counts::Matrix{Float64}
    miss_counts::Union{Matrix{Float64},Nothing}
    self_miss_counts::Union{Matrix{Float64},Nothing}
    row_connection_counts::Matrix{Float64}
    row_self_connection_counts::Matrix{Float64}
    row_miss_counts::Union{Matrix{Float64},Nothing}
    row_self_miss_counts::Union{Matrix{Float64},Nothing}
    unit_labels::Vector{String}
    codes::Vector{String}
    connection_names::Vector{String}
    adjacency_key::Vector{Tuple{String,String}}
    meta::NamedTuple
    ordered::Bool
    self_connections::Bool
    weight_by::String
end

function Base.show(io::IO, a::TMAAccumulation)
    print(io, "TMAAccumulation(", length(a.unit_labels), " units × ", length(a.connection_names),
          " connections, ", a.ordered ? "ordered" : "unordered", ")")
end

function _connection_key(codes::Vector{String}, ordered::Bool, diagonal::Bool)
    p = length(codes)
    ordered && return [(codes[i], codes[j]) for j in 1:p for i in 1:p]
    [Tuple(String.(split(n, " & "; limit = 2))) for n in LibQE.connection_names(codes; diagonal)]
end

"""
    TMA.accumulate(data, units, conversations, codes;
                   tensor=nothing, time_col=nothing, ordered=false, binary=true,
                   weight_by=nothing, misses=true, self_connections=false,
                   split_by=nothing) -> TMAAccumulation

Accumulate each unit's network (= R tma's `accumulate()`; qe-tma's
`tma.accumulate()`). `data` is any Tables.jl table; `units`, `conversations`,
`split_by` are column names (Symbol / String, or vectors of them); `codes` the
code columns.

- `tensor` — a [`context_tensor`](@ref); default: window 1, weight 1.
- `time_col` — timestamps; without one, each line's position within its
  context (1, 2, …; R's context row id), so windows count the lines a unit can
  see, not rows of the whole data.
- `ordered` — directed (ONA) networks, else undirected (ENA).
- `weight_by` — `"binary"`, `"product"`, `"sqrt"` or `"log1p"`, applied per
  line before the unit sum; default `"binary"` if `binary`, else `"product"`.
- `misses` — also accumulate missed co-occurrences (default `true`).
- `self_connections` — unordered only: each code's self-connection as a
  connection column (A & A, …).
- `split_by` — split each unit's context by these columns (rENA splits by the
  conversation); windows never cross a split.
"""
function accumulate(data, units, conversations, codes;
                    tensor::Union{ContextTensor,Nothing} = nothing, time_col = nothing,
                    ordered::Bool = false, binary::Bool = true, weight_by = nothing,
                    misses::Bool = true, self_connections::Bool = false, split_by = nothing)
    weight_by === nothing && (weight_by = binary ? "binary" : "product")
    weight_by isa AbstractString ||
        throw(ArgumentError("weight_by must be one of \"binary\", \"product\", \"sqrt\", \"log1p\""))
    cols = Tables.columntable(data)
    codesyms = _symbols(codes)
    codenames = String.(codesyms)
    p = length(codesyms)
    diagonal = self_connections && !ordered
    ncon = ordered ? p * p : LibQE.n_connections(p; diagonal)

    cm = contexts(cols; units, conversations, split_by)
    n = length(first(cols))
    t = tensor === nothing ? context_tensor(cols) : tensor
    flat, dims = vec(t.array), _tensor_dims(t)
    si, ri, mi = _tensor_inds(t)
    lookup = _context_lookup(t, cols, n)
    codemat = Float64[Float64(cols[c][i]) for i in 1:n, c in codesyms]
    times = time_col === nothing ? nothing : Float64.(cols[Symbol(time_col)])

    nu = length(cm.unit_labels)
    tab(r, c, keep = true) = keep ? zeros(r, c) : nothing
    cc, sc = tab(nu, ncon), tab(nu, p)
    mc, smc = tab(nu, ncon, misses), tab(nu, p, misses)
    rcc, rsc = tab(n, ncon), tab(n, p)
    rmc, rsmc = tab(n, ncon, misses), tab(n, p, misses)

    for (u, label) in enumerate(cm.unit_labels)
        own = Set(cm.unit_own_rows[label])
        for ctx_rows in cm.splits_map[label]
            own_here = filter(in(own), ctx_rows)
            isempty(own_here) && continue
            visible = ctx_rows[1:findfirst(==(last(own_here)), ctx_rows)]   # up to the unit's last line
            local_rows = Int32[findfirst(==(r), visible) - 1 for r in own_here]   # 0-based
            ts = times === nothing ? collect(1.0:length(visible)) : times[visible]
            res = accumulate_unit_networks(codemat[visible, :], flat, dims, si, ri, mi,
                                           lookup[visible, :], local_rows, ts;
                                           ordered, weight = weight_by, misses, diagonal)
            # Unit vectors are additive over lines: sum the split contexts.
            cc[u, :] .+= res.connection_counts
            sc[u, :] .+= res.self_connection_counts
            rcc[own_here, :] .= res.row_connection_counts
            rsc[own_here, :] .= res.row_self_connection_counts
            if misses
                mc[u, :] .+= res.miss_counts
                smc[u, :] .+= res.self_miss_counts
                rmc[own_here, :] .= res.row_miss_counts
                rsmc[own_here, :] .= res.row_self_miss_counts
            end
        end
    end

    key = _connection_key(codenames, ordered, diagonal)
    metacols = [c for c in propertynames(cols) if !(c in codesyms)]
    first_rows = [cm.unit_own_rows[l][1] for l in cm.unit_labels]
    meta = NamedTuple{Tuple(metacols)}(Tuple(cols[c][first_rows] for c in metacols))
    TMAAccumulation(cc, sc, mc, smc, rcc, rsc, rmc, rsmc, cm.unit_labels, codenames,
                    ["$a & $b" for (a, b) in key], key, meta, ordered, diagonal, String(weight_by))
end
