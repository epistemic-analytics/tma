/**
 * @qe-libs/tma-wasm — libtma (tma's accumulation C++) compiled to WebAssembly.
 *
 * Usage (Node.js or bundler):
 *
 *   import loadTMA from '@qe-libs/tma-wasm';
 *   const tma = await loadTMA();
 *
 *   const codes = new Float64Array([1,1,0, 1,0,1, 0,1,1]);
 *   const out   = tma.accumulate_stanza(codes, 3, 3, 2, 0, true, false);
 *   // out → { data: Float64Array, rows: 3, cols: 3 }
 *
 * Functions (moved from @qe-libs/libqe-wasm, same signatures):
 *   connection_matrix, accumulate_stanza, row_connections, rolling_window_sum,
 *   flat_index, accumulate_unit, accumulate_unit_with_rows,
 *   accumulate_tensor_unit, aggregate_row_connections, finalize_row_connections
 *
 * Also: accumulate_tensor_unit_misses — same arguments as
 * accumulate_tensor_unit; the result adds miss_counts and row_miss_counts
 * (ground code i present while response code j is absent, directed n² rows).
 *
 * Matrix convention: inputs are flat row-major Float64Arrays with explicit
 * rows and cols; matrix results are { data: Float64Array, rows, cols }, so a
 * cell is data[row * cols + col].
 */
import createLibTMA from '../dist/libtma.js';

export default async function loadTMA(...args) {
    return createLibTMA(...args);
}
