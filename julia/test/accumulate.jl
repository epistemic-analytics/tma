# TMA.accumulate (src/accumulate.jl) against R tma's accumulate(): the R
# reference values qe-tma's tests use (python/tests/test_tma.py) and R tma's
# fixture on interleaved conversations (fixtures/contexts-split.json, from
# python/tests/fixtures/contexts-split.R: unsplit and split_by conversation).
using JSON

# Unit z has a single line (A and B) in its own conversation. R reference:
# accumulate(contexts(toy, units_by = "u", hoo_rules = conversation_rules((c %in% UNIT$c))),
#            c("A","B","C"), context_tensor(toy, NULL, NULL, default_window = 3), weight_by = "product")
const TOY = (u = ["x", "x", "x", "y", "y", "z", "w", "w"],
             c = ["1", "1", "1", "2", "2", "3", "4", "4"],
             A = [2.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0],
             B = [0.0, 3.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0],
             C = [1.0, 0.0, 2.0, 0.0, 1.0, 0.0, 1.0, 0.0])
toy_acc(; kw...) = TMA.accumulate(TOY, :u, :c, [:A, :B, :C];
                                  tensor = context_tensor(TOY; default_window = 3), kw...)
unit(a, l, field = :connection_counts) = getfield(a, field)[findfirst(==(l), a.unit_labels), :]

@testset "TMA.accumulate matches R tma" begin
    a = toy_acc(weight_by = "product")
    @test a.unit_labels == ["x", "y", "z", "w"]
    for (l, ref) in ("x" => [12, 9, 12], "y" => [2, 2, 1], "z" => [1, 0, 0], "w" => [2, 1, 2])
        @test unit(a, l) ≈ ref
    end
    o = toy_acc(ordered = true, weight_by = "product")   # A & A, B & A, C & A, A & B, ...
    for (l, ref) in ("x" => [2, 1.5, 2, 10.5, 3, 5, 7, 7, 2], "y" => [1, 1.5, 0.5, 0.5, 0, 0, 1.5, 1, 0],
                     "z" => [0, 0.5, 0, 0.5, 0, 0, 0, 0, 0], "w" => [0, 1.5, 1, 0.5, 1, 1.5, 0, 0.5, 0])
        @test unit(o, l) ≈ ref
    end
    @test o.connection_names[1:4] == ["A & A", "B & A", "C & A", "A & B"]
    @test a.connection_names == ["A & B", "A & C", "B & C"]
    @test a.adjacency_key == [("A", "B"), ("A", "C"), ("B", "C")]
end

@testset "TMA.accumulate weight models, misses, self-connections" begin
    # rENA's legacy ena.accumulate.data(weight.by = ...), unit x (python TestWeightBy)
    w = (u = ["x", "x", "x", "y", "y", "z", "z"], c = ["1", "1", "1", "2", "2", "3", "3"],
         A = [2.0, 1, 0, 1, 1, 0, 1], B = [0.0, 3, 1, 1, 0, 1, 1], C = [1.0, 0, 2, 0, 1, 1, 0])
    wa(; kw...) = unit(TMA.accumulate(w, :u, :c, [:A, :B, :C];
                                      tensor = context_tensor(w; default_window = 3), kw...), "x")
    @test wa(weight_by = "binary") ≈ [2, 3, 2]
    @test wa(weight_by = "product") ≈ [12, 9, 12]
    @test wa(weight_by = "sqrt") ≈ [4.73205080756888, 4.86370330515627, 4.73205080756888]
    @test wa(weight_by = "log1p") ≈ [3.68887945411394, 3.73766961828337, 3.68887945411394]
    @test wa() == wa(weight_by = "binary") && wa(binary = false) == wa(weight_by = "product")
    @test_throws ArgumentError wa(weight_by = sqrt)

    m(; kw...) = unit(TMA.accumulate(w, :u, :c, [:A, :B, :C];
                                     tensor = context_tensor(w; default_window = 3), kw...), "x", :miss_counts)
    @test m(weight_by = "product") ≈ [3, 3, 0]
    @test m(weight_by = "binary") ≈ [1, 2, 0]
    @test m(ordered = true) ≈ [3, 3, 1, 0, 0, 0, 2, 0, 1]
    @test toy_acc(misses = false).miss_counts === nothing

    @test unit(toy_acc(weight_by = "product"), "x", :self_connection_counts) ≈ [2, 3, 2]
    @test unit(toy_acc(weight_by = "product"), "x", :self_miss_counts) ≈ [3, 0, 1]
    o = toy_acc(ordered = true, weight_by = "product")
    @test o.self_connection_counts ≈ o.connection_counts[:, [1, 5, 9]]   # the ordered diagonal

    s = toy_acc(self_connections = true, weight_by = "product")
    @test s.self_connections
    @test s.connection_names == ["A & A", "A & B", "B & B", "A & C", "B & C", "C & C"]
    @test unit(s, "x") ≈ [2, 12, 3, 9, 12, 2]
    @test s.connection_counts[:, [1, 3, 6]] ≈ s.self_connection_counts
    @test !toy_acc(ordered = true, self_connections = true).self_connections
end

@testset "TMA.accumulate meta and per-line tables" begin
    a = toy_acc(weight_by = "product")
    @test collect(keys(a.meta)) == [:u, :c]
    @test a.meta.u == a.unit_labels
    @test size(a.row_connection_counts) == (8, 3)
    for (i, l) in enumerate(a.unit_labels)
        @test vec(sum(a.row_connection_counts[TOY.u .== l, :]; dims = 1)) ≈ a.connection_counts[i, :]
    end
end

@testset "TMA.accumulate: R tma on interleaved conversations (unsplit / split_by)" begin
    fx = JSON.parsefile(joinpath(@__DIR__, "fixtures", "contexts-split.json"))
    t = fx["toy"]
    data = (u = String.(t["u"]), c = String.(t["c"]),
            A = Float64.(t["A"]), B = Float64.(t["B"]), C = Float64.(t["C"]))
    variants = Dict("binary" => (;), "product" => (weight_by = "product",),
                    "ordered" => (ordered = true, weight_by = "product"))
    for (name, ref) in fx["cases"]
        how, window, variant = split(name, "_")
        a = TMA.accumulate(data, :u, :c, [:A, :B, :C];
                           tensor = context_tensor(data; default_window = parse(Float64, window)),
                           split_by = how == "split" ? :c : nothing, misses = false,
                           variants[variant]...)
        order = [findfirst(==(l), a.unit_labels) for l in ref["units"]]
        want = permutedims(reduce(hcat, [Float64.(r) for r in ref["counts"]]))
        @test a.connection_counts[order, :] ≈ want
    end
    # Splits: one group per conversation, first appearance, covering the context
    cm = contexts(data; units = :u, conversations = :c, split_by = :c)
    for (l, groups) in cm.splits_map
        @test sort(reduce(vcat, groups)) == cm.contexts_map[l]
        @test all(length(unique(data.c[g])) == 1 for g in groups)
        @test issorted(first.(groups))
    end
    cu = contexts(data; units = :u, conversations = :c)                 # unsplit: one context
    @test all(cu.splits_map[l] == [cu.contexts_map[l]] for l in cu.unit_labels)
end
