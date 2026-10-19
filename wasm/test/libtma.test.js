/**
 * Tests for @qe-libs/tma-wasm (dist/libtma.*): accumulation.  Run
 * `npm run build` first.
 *
 * The first part moved from libqe-wasm's test/basic.test.js with the code
 * (libqe's phase 4b split), unchanged.  The parity block checks that this
 * module and libqe-wasm's own copy (removed in libqe 0.2.0; skipped then)
 * give identical results.
 */
import loadTMA from '../js/index.js';
import loadLibQE from '@qe-libs/libqe-wasm';

let qe;
beforeAll(async () => { qe = await loadTMA(); });

// ── Accumulation ─────────────────────────────────────────────────────────────

test('accumulate_stanza: output shape matches input rows', () => {
    // 3 rows × 3 codes → choose_two(3)=3 connection columns
    const codes = new Float64Array([1,1,0, 1,0,1, 0,1,1]);
    const out   = qe.accumulate_stanza(codes, 3, 3, 2, 0, true);
    expect(out.rows).toBe(3);
    expect(out.cols).toBe(3);
});

test('rolling_window_sum: single row window is identity', () => {
    const codes = new Float64Array([1,2,3, 4,5,6, 7,8,9]);
    const out   = qe.rolling_window_sum(codes, 3, 3, 1);
    expect(Array.from(out.data)).toEqual([1,2,3, 4,5,6, 7,8,9]);
});

test('flat_index matches column-major expectation', () => {
    // 3D array of dims [2,3,4], index [1,2,3] → 1 + 2*2 + 3*6 = 23
    expect(qe.flat_index([1,2,3], [2,3,4])).toBe(23);
});

// ── Accumulation — extended ───────────────────────────────────────────────────

test('accumulate_stanza: ordered=true gives n_codes² columns', () => {
    // 3 rows × 3 codes, ordered → 3² = 9 cols
    const codes = new Float64Array([1,1,0, 1,0,1, 0,1,1]);
    const out   = qe.accumulate_stanza(codes, 3, 3, 2, 0, true, true);
    expect(out.rows).toBe(3);
    expect(out.cols).toBe(9);
});

test('connection_matrix: ordered=false result is symmetric', () => {
    const g = new Float64Array([1, 0, 1]);
    const r = new Float64Array([1, 1, 0]);
    const m = qe.connection_matrix(g, 3, r, 3, 1.0, false);
    // unordered result is symmetric: m[i,j] == m[j,i] (row-major, 3×3)
    expect(m.data[1]).toBeCloseTo(m.data[3], 10);   // (0,1) vs (1,0)
    expect(m.data[2]).toBeCloseTo(m.data[6], 10);   // (0,2) vs (2,0)
    expect(m.data[5]).toBeCloseTo(m.data[7], 10);   // (1,2) vs (2,1)
});

test('accumulate_unit: output length is choose_two(n_codes) when unordered', () => {
    // 4 rows × 3 codes; unit covers rows [0, 1, 2]; decay = constant 1
    const codes    = new Float64Array([1,0,0, 0,1,0, 0,0,1, 1,1,0]);
    const unitRows = new Int32Array([0, 1, 2]);
    const decayFn  = (d) => new Float64Array(d.length).fill(1.0);
    const out = qe.accumulate_unit(codes, 4, 3, unitRows, decayFn, false);
    // choose(3,2) = 3
    expect(out.length).toBe(3);
});

test('accumulate_unit: output length is n_codes² when ordered', () => {
    const codes    = new Float64Array([1,0,0, 0,1,0, 0,0,1, 1,1,0]);
    const unitRows = new Int32Array([0, 1, 2]);
    const decayFn  = (d) => new Float64Array(d.length).fill(1.0);
    const out = qe.accumulate_unit(codes, 4, 3, unitRows, decayFn, true);
    expect(out.length).toBe(9);
});

test('accumulate_unit_with_rows: networks + row_networks returned', () => {
    const codes    = new Float64Array([1,0, 0,1, 1,1]);
    const unitRows = new Int32Array([0, 1, 2]);
    const decayFn  = (d) => new Float64Array(d.length).fill(1.0);
    const out = qe.accumulate_unit_with_rows(codes, 3, 2, unitRows, decayFn, false);
    // choose(2,2) = 1 connection
    expect(out.networks.length).toBe(1);
    // row_networks: always 3 rows × n_codes² cols (full square, before folding)
    expect(out.row_networks.rows).toBe(3);
    expect(out.row_networks.cols).toBe(4);   // 2² = 4 for n_codes=2
});

test('accumulate_tensor_unit: default 1-D tensor smoke test', () => {
    // Default tensor: dims=[2], tensor=[weight=1, window=2].
    // IS_DEFAULT path: every ground row within 2 time-units gets weight=1.
    //
    // 4 context rows × 2 codes, times = [0,1,2,3], all rows belong to the unit.
    // No extra context factors — supply a 4×1 all-zeros context_lookup (values
    // are only read in the non-default path, so content doesn't matter here).

    const tensor  = new Float64Array([1, 2]);   // [weight=1, window=2]
    const dims    = new Int32Array([2]);         // 1-D tensor of size 2

    // No extra context factors — supply a single dummy column of zeros
    const cl      = new Int32Array([0, 0, 0, 0]);  // 4×1, all zeros
    const urows   = new Int32Array([0, 1, 2, 3]);
    const codes   = new Float64Array([1,0, 0,1, 1,1, 0,0]);
    const times   = new Float64Array([0, 1, 2, 3]);

    const out = qe.accumulate_tensor_unit(
        tensor, dims,
        new Int32Array([]),   // dims_sender  (none)
        new Int32Array([]),   // dims_receiver (none)
        new Int32Array([]),   // dims_mode    (none)
        cl, 4, 1,
        urows, codes, 4, 2, times,
        /*ordered=*/true
    );

    // connection_counts: length n_codes² = 4
    expect(out.connection_counts.length).toBe(4);
    // row_connection_counts: n_unit_rows × n_codes² = 4×4
    expect(out.row_connection_counts.rows).toBe(4);
    expect(out.row_connection_counts.cols).toBe(4);
});

test('accumulate_tensor_unit_misses: ground present, response absent', () => {
    // tma's R test-misses.R fixture: window 3, weight 1.
    //   row 2 misses A->C 2, C->C 1; row 3 misses A->A 3, B->A 3, C->A 1
    const args = [
        new Float64Array([1, 3]), new Int32Array([2]),
        new Int32Array([]), new Int32Array([]), new Int32Array([]),
        new Int32Array([0, 0, 0]), 3, 1,
        new Int32Array([0, 1, 2]),
        new Float64Array([2, 0, 1,  1, 3, 0,  0, 1, 2]), 3, 3,
        new Float64Array([0, 1, 2]),
        /*ordered=*/true,
    ];
    const out = qe.accumulate_tensor_unit_misses(...args);
    expect(out.row_miss_counts.rows).toBe(3);
    expect(out.row_miss_counts.cols).toBe(9);
    expect(Array.from(out.row_miss_counts.data)).toEqual([
        0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 2, 0, 1,
        3, 3, 1, 0, 0, 0, 0, 0, 0,
    ]);
    expect(Array.from(out.miss_counts)).toEqual([3, 3, 1, 0, 0, 0, 2, 0, 1]);

    const plain = qe.accumulate_tensor_unit(...args);
    expect(plain.miss_counts).toBeUndefined();
    expect(Array.from(plain.row_connection_counts.data))
        .toEqual(Array.from(out.row_connection_counts.data));
});

test('accumulate_unit_networks: finished vectors in one call', () => {
    const args = [
        new Float64Array([1, 3]), new Int32Array([2]),
        new Int32Array([]), new Int32Array([]), new Int32Array([]),
        new Int32Array([0, 0, 0]), 3, 1,
        new Int32Array([0, 1, 2]),
        new Float64Array([2, 0, 1,  1, 3, 0,  0, 1, 2]), 3, 3,
        new Float64Array([0, 1, 2]),
    ];
    const u = qe.accumulate_unit_networks(...args, false, 'product', true);
    expect(Array.from(u.connection_counts)).toEqual([12, 9, 12]);
    expect(u.row_connection_counts.cols).toBe(3);
    expect(Array.from(u.self_connection_counts)).toEqual([2, 3, 2]);
    expect(Array.from(u.miss_counts)).toEqual([3, 3, 0]);
    expect(Array.from(u.self_miss_counts)).toEqual([3, 0, 1]);
    const off = qe.accumulate_unit_networks(...args, true, 'binary', false);
    expect(off.connection_counts.length).toBe(9);
    expect(off.miss_counts).toBeUndefined();
});

test('finalize/aggregate_self_connections: diagonal of each directed row', () => {
    // 2 rows x 2 codes, directed 2x2 per row (column-major): diagonal at 0 and 3.
    const rc = new Float64Array([1, 2, 3, 4,  5, 0, 0, 2]);
    const fin = qe.finalize_self_connections(rc, 2, 4, 2, true, 'product');
    expect(fin.rows).toBe(2);
    expect(fin.cols).toBe(2);
    expect(Array.from(fin.data)).toEqual([1, 4, 5, 2]);
    expect(Array.from(qe.finalize_self_connections(rc, 2, 4, 2, false, 'binary').data))
        .toEqual([1, 1, 1, 1]);
    const agg = Array.from(qe.aggregate_self_connections(rc, 2, 4, 2, false, 'sqrt'));
    expect(agg[0]).toBeCloseTo(1 + Math.sqrt(5), 12);
    expect(agg[1]).toBeCloseTo(2 + Math.sqrt(2), 12);
});

// ── aggregate_row_connections / finalize_row_connections (weight models) ─────

// Two response rows × 3 codes, directed 3×3 per row (column-major within the
// row), row-major overall — counts > 1 and asymmetric (same fixture as R tests).
function weightedRowConn() {
    const rc = new Float64Array(2 * 9);
    rc[0 * 9 + 3] = 2;    // m(0,1)
    rc[0 * 9 + 1] = 1;    // m(1,0)  → row 0 folds to A&B = 3
    rc[0 * 9 + 7] = 4;    // m(1,2)  → row 0 B&C = 4
    rc[1 * 9 + 6] = 9;    // m(0,2)  → row 1 A&C = 9
    rc[1 * 9 + 3] = 0.5;  // m(0,1)  → row 1 A&B = 0.5
    return rc;
}
const agg = (w, ordered = false) =>
    Array.from(qe.aggregate_row_connections(weightedRowConn(), 2, 9, 3, ordered, w));
const expectClose = (got, want) => want.forEach((v, i) => expect(got[i]).toBeCloseTo(v, 12));

test('aggregate_row_connections: unordered weight models apply per row before the sum', () => {
    expectClose(agg('binary'),  [2, 1, 1]);
    expectClose(agg('product'), [3.5, 9, 4]);
    expectClose(agg('sqrt'),    [Math.sqrt(3) + Math.sqrt(0.5), 3, 2]);
    expectClose(agg('log1p'),   [Math.log1p(3) + Math.log1p(0.5), Math.log1p(9), Math.log1p(4)]);
    expectClose(agg('log'),     agg('log1p'));
});

test('aggregate_row_connections: legacy boolean flag maps to binary / product', () => {
    expect(agg(true)).toEqual(agg('binary'));
    expect(agg(false)).toEqual(agg('product'));
});

test('aggregate_row_connections: ordered binary keeps raw directed counts', () => {
    const rc = weightedRowConn();
    const colSum = f => Array.from({ length: 9 }, (_, c) => f(rc[c]) + f(rc[9 + c]));
    expectClose(agg('binary', true),  colSum(x => x));
    expectClose(agg('product', true), colSum(x => x));
    expectClose(agg('sqrt', true),    colSum(Math.sqrt));
});

test('finalize_row_connections: per-row output sums to the aggregate', () => {
    for (const w of ['binary', 'product', 'sqrt', 'log1p']) {
        for (const ordered of [false, true]) {
            const fin = qe.finalize_row_connections(weightedRowConn(), 2, 9, 3, ordered, w);
            expect(fin.rows).toBe(2);
            expect(fin.cols).toBe(ordered ? 9 : 3);
            const sums = Array.from({ length: fin.cols }, (_, c) => fin.data[c] + fin.data[fin.cols + c]);
            expectClose(sums, agg(w, ordered));
        }
    }
});

test('aggregate_row_connections: rejects unknown weight model names', () => {
    expect(() => agg('cube')).toThrow();
});

test('aggregate_row_connections: tensor rows match rENA ena.accumulate.data weight.by', () => {
    // One unit/conversation, window 3 (same example pinned in the R tests).
    const out = qe.accumulate_tensor_unit(
        new Float64Array([1, 3]), new Int32Array([2]),
        new Int32Array([]), new Int32Array([]), new Int32Array([]),
        new Int32Array([0, 0, 0]), 3, 1,
        new Int32Array([0, 1, 2]),
        new Float64Array([2, 0, 1,  1, 3, 0,  0, 1, 2]), 3, 3,
        new Float64Array([0, 1, 2]),
        /*ordered=*/true
    );
    const rc = out.row_connection_counts;
    const a  = w => Array.from(qe.aggregate_row_connections(rc.data, rc.rows, rc.cols, 3, false, w));
    expectClose(a('binary'),  [2, 3, 2]);
    expectClose(a('product'), [12, 9, 12]);
    expectClose(a('sqrt'),    [4.73205080756888, 4.86370330515627, 4.73205080756888]);
    expectClose(a('log1p'),   [3.68887945411394, 3.73766961828337, 3.68887945411394]);
});

// ── parity with libqe-wasm ────────────────────────────────────────────────────

describe('libtma module vs libqe-wasm', () => {
    let libqe;
    beforeAll(async () => { libqe = await loadLibQE(); });

    const codes   = new Float64Array([1,0,2, 0,1,1, 2,1,0, 1,1,1, 0,2,1]);   // 5 × 3
    const binary  = new Float64Array([1,0,1, 0,1,1, 1,1,0, 1,1,1, 0,1,1]);
    const rowConn = new Float64Array([0,1,0, 2,0,4, 0,0,0,  0,0,9, 0.5,0,0, 1,0,0]);  // 2 × 9
    const decay   = d => d.map(x => (x < 3 ? 1 : 0));
    const cases = {
        connection_matrix:        m => m.connection_matrix([1,0,2], 3, [0,1,1], 3, 0.5, true),
        accumulate_stanza:        m => m.accumulate_stanza(codes, 5, 3, 3, 1, false, false),
        accumulate_stanza_ordered: m => m.accumulate_stanza(binary, 5, 3, 3, 0, true, true),
        row_connections:          m => m.row_connections(codes, 5, 3, false),
        rolling_window_sum:       m => m.rolling_window_sum(codes, 5, 3, 3),
        flat_index:               m => m.flat_index([1, 2, 3], [2, 3, 4]),
        accumulate_unit:          m => m.accumulate_unit(codes, 5, 3, [2, 4], decay, false),
        accumulate_unit_with_rows: m => m.accumulate_unit_with_rows(codes, 5, 3, [2, 4], decay, true),
        accumulate_tensor_unit:   m => m.accumulate_tensor_unit([1, 3], [2], [], [], [],
                                        new Int32Array(0), 5, 0, [2, 3, 4], codes, 5, 3,
                                        [0, 1, 2, 3, 4], true),
        aggregate_row_connections: m => m.aggregate_row_connections(rowConn, 2, 9, 3, false, 'sqrt'),
        finalize_row_connections: m => m.finalize_row_connections(rowConn, 2, 9, 3, true, 'log1p'),
    };

    // libqe-wasm >= 0.2.0 no longer has these functions; compare only those it
    // still provides (none, once 0.2 is installed).
    test.each(Object.keys(cases))('%s matches libqe-wasm', name => {
        const fn = name.replace(/_ordered$/, '');
        if (typeof libqe[fn] !== 'function') return;
        expect(cases[name](qe)).toEqual(cases[name](libqe));
    });
});
