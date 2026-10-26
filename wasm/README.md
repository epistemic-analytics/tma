# @qe-libs/tma-wasm

WebAssembly build of libtma — [tma](https://gitlab.com/epistemic-analytics/qe-packages/tma)'s
accumulation C++ (stanza windows, context tensors, row connections, weight
models), shared with the R package and qe-tma (Python). These functions moved
here from `@qe-libs/libqe-wasm` (libqe's phase 4b split) with the same
signatures and results.

This package is the kernel layer only: one unit's networks at a time, from
arrays you prepare. For the full pipeline in JavaScript (data parsing, units
and conversations, context tensors, horizons of observation, then the ENA
model: normalization, rotation, points and node positions), use
[`@qe-libs/rena-wasm`](https://qe-libs.org/npm/qe-libs-rena-wasm/). It compiles
these same functions in, and its models carry the connection, miss and
self-connection counts that tma's `accumulate()` returns in R.

## Installation

```bash
# one-time: tell npm where to find the @qe-libs scope
echo "@qe-libs:registry=https://gitlab.com/api/v4/projects/22522458/packages/npm/" >> ~/.npmrc
npm install @qe-libs/tma-wasm
```

## Usage

On [qe-libs.org/npm/qe-libs-tma-wasm/](https://qe-libs.org/npm/qe-libs-tma-wasm/)
the `js run` examples have a **Run** button: they run in your browser on this
WebAssembly build, with `console.log` printed under the example.

```js run
import loadTMA from '@qe-libs/tma-wasm';
const tma = await loadTMA();

// Stanza-window accumulation: 3 rows × 3 codes, window 2
const codes = new Float64Array([1,1,0, 1,0,1, 0,1,1]);
const out = tma.accumulate_stanza(codes, 3, 3, 2, 0, true, false);
// → { data: Float64Array, rows: 3, cols: 3 }   (choose_two(3) connections)
console.log('stanza:', out);

// Context-tensor accumulation for one unit, then the unit vector with a weight model
const t = tma.accumulate_tensor_unit([1, 3], [2], [], [], [], new Int32Array(0), 3, 0,
                                     [0, 1, 2], codes, 3, 3, [0, 1, 2], true);
const v = tma.aggregate_row_connections(t.row_connection_counts.data,
                                        t.row_connection_counts.rows,
                                        t.row_connection_counts.cols, 3, false, 'sqrt');
console.log('row connections:', t.row_connection_counts);
console.log('unit vector (sqrt):', v);
```

Matrices are flat row-major `Float64Array`s with explicit `rows`/`cols`;
matrix results are `{ data, rows, cols }`.

| Function | Notes |
|----------|-------|
| `connection_matrix(ground, gn, response, rn, response_weight, ordered)` | one ground/response pair |
| `accumulate_stanza(data, rows, cols, window_back, window_forward, binary, ordered)` | rENA stanza model |
| `row_connections(data, rows, cols, binary)` | per-row co-occurrence |
| `rolling_window_sum(data, rows, cols, window_size)` | backward window sum |
| `flat_index(indices, dims)` | column-major linear index |
| `accumulate_unit(codes, rows, cols, unit_rows, decay_fn, ordered)` | ground/response model, JS decay |
| `accumulate_unit_with_rows(...)` | the same, with per-row networks |
| `accumulate_tensor_unit(tensor, dims, …, codes, rows, cols, times, ordered)` | context-tensor model, raw directed rows |
| `accumulate_tensor_unit_misses(…same arguments…)` | the same, plus `miss_counts` / `row_miss_counts` |
| `accumulate_unit_networks(…, times, ordered, weight, misses[, diagonal])` | one unit's finished networks, as tma's `accumulate()` |
| `aggregate_row_connections(row_conn, rows, cols, n_codes, ordered, weight[, diagonal])` | fold + weight model + sum |
| `finalize_row_connections(row_conn, rows, cols, n_codes, ordered, weight[, diagonal])` | per-row step of the above |
| `weight_rows(rows_data, rows, cols, ordered, weight)` | weight model per line on already-folded rows |
| `aggregate_self_connections(row_conn, rows, cols, n_codes, ordered, weight)` | unit self-connection vector (n_codes) |
| `finalize_self_connections(row_conn, rows, cols, n_codes, ordered, weight)` | per-row step of the above |

`weight` is `'binary' | 'product' | 'sqrt' | 'log1p'` (or the legacy boolean).
`diagonal` (unordered only) keeps self-connections as columns: `A & A`,
`A & B`, `B & B`, ….

`accumulate_unit_networks` takes `accumulate_tensor_unit`'s arguments plus
`weight`, `misses` and an optional `diagonal`, and returns the same results as
tma's `accumulate()` for one unit:

| Field | R `accumulate()` |
|-------|------------------|
| `connection_counts`, `row_connection_counts` | `connection.counts`, `model$row.connection.counts` |
| `self_connection_counts`, `row_self_connection_counts` | `self.connection.counts`, `model$row.self.connection.counts` |
| `miss_counts`, `row_miss_counts` (with `misses`) | `miss.counts`, `model$row.miss.counts` |
| `self_miss_counts`, `row_self_miss_counts` (with `misses`) | `self.miss.counts`, `model$row.self.miss.counts` |

Unit vectors have the weight model applied per line; `row_*` matrices are raw.
R's `vignette("accumulation-results", package = "tma")` explains what each of
them counts. Pass `row_miss_counts` to the self-connection functions to get
self-misses.

## Building and testing

```bash
npm install
npm run build   # compiles dist/libtma.* — needs emscripten (emcc) and conan
npm test
```

`npm run build` (`scripts/build.sh`) vendors the libtma and libqe headers into
`include/` (`../scripts/sync-headers.sh`: libtma from `inst/include`, libqe from
Conan), gets Armadillo from Conan, and builds with `emcmake`.
`test/libtma.test.js` checks this module against libqe-wasm's own copy of the
accumulation functions.
