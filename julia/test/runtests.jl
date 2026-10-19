using Test
using LinearAlgebra
using TMA

# ── Moved from LibQE.jl's tests with the code (libqe's phase 4b split) ────────

# ── Accumulation ─────────────────────────────────────────────────────────────

@testset "accumulate_stanza shape" begin
    codes = Float64[1 1 0; 1 0 1; 0 1 1]   # 3 rows × 3 codes
    out   = accumulate_stanza(codes; window_back=2, binary=true)
    @test size(out) == (3, 3)              # 3 rows × choose_two(3)
    @test all(out .∈ Ref([0.0, 1.0]))      # binary output
end

@testset "row_connections" begin
    codes = Float64[1 0 1; 0 1 1]
    out   = row_connections(codes)
    @test size(out) == (2, 3)             # 2 rows × choose_two(3)
end

@testset "rolling_window_sum identity at window=1" begin
    codes = Float64[1 2 3; 4 5 6; 7 8 9]
    out   = rolling_window_sum(codes; window_size=1)
    @test out ≈ codes
end

@testset "flat_index" begin
    # 3D array dims [2,3,4], 0-based index [1,2,3] → 1 + 2*2 + 3*6 = 23
    @test flat_index(Int32[1, 2, 3], Int32[2, 3, 4]) == 23
end

@testset "accumulate_unit with uniform decay" begin
    codes     = Matrix{Float64}(I, 3, 3)
    unit_rows = Int32[1, 2]
    decay     = dists -> ones(Float64, length(dists))
    result    = accumulate_unit(codes, unit_rows, decay; ordered=false)
    @test length(result) == 3     # choose_two(3)
end

# ── Added with the move: functions LibQE.jl's tests did not cover ─────────────

@testset "connection_matrix" begin
    g, r = [1.0, 0.0, 2.0], [0.0, 1.0, 1.0]
    d = connection_matrix(g, r; ordered = true)
    @test size(d) == (3, 3)
    u = connection_matrix(g, r; ordered = false)
    @test u ≈ u'                                   # undirected → symmetric
end

@testset "accumulate_stanza ordered gives n_codes² columns" begin
    codes = Float64[1 1 0; 1 0 1; 0 1 1]
    @test size(accumulate_stanza(codes; window_back = 2, ordered = true)) == (3, 9)
end

@testset "accumulate_unit_with_rows" begin
    codes = Float64[1 0 1; 0 1 1; 1 1 0; 1 0 0]
    decay = d -> ones(Float64, length(d))
    r = accumulate_unit_with_rows(codes, Int32[1, 2, 3], decay; ordered = false)
    @test length(r.networks) == 3                  # choose_two(3)
    @test size(r.row_networks) == (3, 9)           # one directed p² row per unit row
    # agrees with accumulate_unit on the same decay
    @test r.networks ≈ accumulate_unit(codes, Int32[1, 2, 3], decay; ordered = false)
end

# Two response rows × 3 codes, directed 3×3 per row (column-major within the
# row) — the fixture tma's R tests and tma-wasm's use for the weight models.
function weighted_rc()
    rc = zeros(2, 9)
    rc[1, 4] = 2;  rc[1, 2] = 1;  rc[1, 8] = 4     # row 1: A&B = 3, B&C = 4
    rc[2, 7] = 9;  rc[2, 4] = 0.5                  # row 2: A&C = 9, A&B = 0.5
    rc
end

@testset "aggregate_row_connections weight models" begin
    rc = weighted_rc()
    @test aggregate_row_connections(rc, 3; weight = "binary")  ≈ [2, 1, 1]
    @test aggregate_row_connections(rc, 3; weight = "product") ≈ [3.5, 9, 4]
    @test aggregate_row_connections(rc, 3; weight = true)      ≈ [2, 1, 1]      # legacy flag
    @test aggregate_row_connections(rc, 3; weight = false)     ≈ [3.5, 9, 4]
    @test length(aggregate_row_connections(rc, 3; ordered = true)) == 9
    fin = finalize_row_connections(rc, 3; weight = "sqrt")
    @test size(fin) == (2, 3)
    @test vec(sum(fin, dims = 1)) ≈ aggregate_row_connections(rc, 3; weight = "sqrt")
end

@testset "accumulate_tensor_unit rows match rENA weight.by" begin
    # One unit/conversation, window 3 (tma's R test of the same name)
    codes = Float64[2 0 1; 1 3 0; 0 1 2]
    r = accumulate_tensor_unit(codes, [1.0, 3.0], Int32[2], Int32[], Int32[], Int32[],
                               zeros(Int32, 3, 0), Int32[0, 1, 2], [0.0, 1.0, 2.0])
    @test size(r.row_connection_counts) == (3, 9)
    rc = r.row_connection_counts
    @test aggregate_row_connections(rc, 3; weight = "binary")  ≈ [2, 3, 2]
    @test aggregate_row_connections(rc, 3; weight = "product") ≈ [12, 9, 12]
    @test aggregate_row_connections(rc, 3; weight = "sqrt") ≈
          [4.73205080756888, 4.86370330515627, 4.73205080756888]
end

@testset "accumulate_unit_networks" begin
    codes = Float64[2 0 1; 1 3 0; 0 1 2]
    args  = (codes, [1.0, 3.0], Int32[2], Int32[], Int32[], Int32[],
             zeros(Int32, 3, 0), Int32[0, 1, 2], [0.0, 1.0, 2.0])
    u = accumulate_unit_networks(args...; weight = "product")
    @test u.connection_counts == [12.0, 9.0, 12.0]
    @test size(u.row_connection_counts) == (3, 3)
    @test u.self_connection_counts == [2.0, 3.0, 2.0]
    @test u.row_self_connection_counts == Float64[0 0 0; 2 0 0; 0 3 2]
    @test u.miss_counts == [3.0, 3.0, 0.0]
    @test u.self_miss_counts == [3.0, 0.0, 1.0]
    off = accumulate_unit_networks(args...; ordered = true, misses = false)
    @test length(off.connection_counts) == 9
    @test !haskey(off, :miss_counts)
end

@testset "weight_rows" begin
    rc = Float64[1 2 3 4; 5 0 0 2]
    raw = finalize_row_connections(rc, 2; weight = "product")
    @test weight_rows(raw; weight = "sqrt") ≈ finalize_row_connections(rc, 2; weight = "sqrt")
    @test weight_rows(rc; ordered = true, weight = "log1p") ≈ finalize_row_connections(rc, 2; ordered = true, weight = "log1p")
end

@testset "finalize/aggregate_self_connections" begin
    rc = Float64[1 2 3 4; 5 0 0 2]     # 2 codes: each row's diagonal at columns 1 and 4
    @test finalize_self_connections(rc, 2; ordered = true, weight = "product") == Float64[1 4; 5 2]
    @test finalize_self_connections(rc, 2; weight = "binary") == Float64[1 1; 1 1]
    @test aggregate_self_connections(rc, 2; weight = "sqrt") ≈ [1 + sqrt(5), 2 + sqrt(2)]
end

@testset "accumulate_tensor_unit misses" begin
    # Same fixture: row 2 misses A->C 2, C->C 1; row 3 misses A->A 3, B->A 3, C->A 1
    codes = Float64[2 0 1; 1 3 0; 0 1 2]
    args  = (codes, [1.0, 3.0], Int32[2], Int32[], Int32[], Int32[],
             zeros(Int32, 3, 0), Int32[0, 1, 2], [0.0, 1.0, 2.0])
    r = accumulate_tensor_unit(args...; misses = true)
    @test r.row_miss_counts == Float64[0 0 0 0 0 0 0 0 0;
                                       0 0 0 0 0 0 2 0 1;
                                       3 3 1 0 0 0 0 0 0]
    @test r.miss_counts == vec(sum(r.row_miss_counts; dims = 1))
    @test aggregate_row_connections(r.row_miss_counts, 3; ordered = false, weight = "product") ≈ [3, 3, 0]
    plain = accumulate_tensor_unit(args...)
    @test !haskey(plain, :miss_counts)
    @test plain.row_connection_counts == r.row_connection_counts
end
