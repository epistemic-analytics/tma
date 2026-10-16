# tma (development version)

* `accumulate()` keeps self-connections: `self.connection.counts` (one column
  per code) and `model$row.self.connection.counts` (raw, per line). A code's
  self-connection on a line is its ground count times its response count -- the
  ordered network's diagonal, which unordered networks drop when they fold to
  the upper triangle -- weighted per line by `weight_by` and summed per unit,
  for ordered and unordered models alike. With `misses = TRUE`, also
  `self.miss.counts` / `model$row.self.miss.counts` (the code in the ground,
  absent from the response). Not used in the model yet.
* libtma `finalize_self_connections()` / `aggregate_self_connections()` compute
  them from `apply_tensor_unit`'s per-line rows, exported in R, qe-tma, tma-wasm
  and TMA.jl. qe-tma `TMA.fit()` stores `self_networks_` (and
  `self_miss_networks_` with `misses=True`).

# tma 0.3.7

* `accumulate(misses = TRUE)` also accumulates missed co-occurrences: for each
  response line, the window-weighted ground count of code i when code j is
  absent from the response, with no response self-term. Misses go through
  the same steps as connection counts: directed for ordered models, folded to
  the upper triangle for unordered ones, `weight_by` applied per line.
  Results land in `miss.counts` and `model$row.miss.counts`. Backed by
  libtma's `miss_matrix()` and `apply_tensor_unit(..., return_misses = true)`;
  `apply_tensor()` gains `misses = FALSE`.
* Misses in every binding: qe-tma `apply_tensor_unit(..., misses=False)` and
  `TMA.fit(misses=False)` (→ `miss_networks_`); tma-wasm
  `accumulate_tensor_unit_misses()` (same arguments as
  `accumulate_tensor_unit`); TMA.jl `accumulate_tensor_unit(...; misses=false)`.
  Results gain `miss_counts` / `row_miss_counts` only when misses are asked
  for, so existing callers see no change.
* qe-tma `TMA.fit()` fixes:
  - `connection_names_` match R's `adjacency_key()`: ordered labels were
    transposed (position j*n+i is ground i -> response j, labelled "i & j"),
    and unordered labels come from libqe's `connection_names` ("A & B", was
    "A&B").
  - Units with a single visible line are no longer skipped: as in R, the
    response self-term connects their co-occurring codes. (A single-row
    selection is read-only under pandas copy-on-write, so the code matrix is
    now copied before it reaches the extension.)
* Works with libqe 0.2.0, which drops its copy of the accumulation code (tma
  no longer uses it). qe-tma 0.1.4 allows `qe-lib<0.3`;
  tma-wasm allows `@qe-libs/libqe-wasm` 0.2 (its libqe-wasm parity tests skip
  the functions 0.2 removed).
* CI installs qe-ena from qe-libs.org instead of building rENA's `develop`.

# tma 0.3.6

* tma is a compiled package again. The accumulation C++ (stanza windows,
  context tensors, row connections, weight models) moved out of libqe into
  tma as the header-only layer libtma (`inst/include/libtma/`, wrapped in
  `src/libtma_rcpp.cpp`), so libqe can return to generic numerics. Results are
  unchanged: the code and its tests moved verbatim. tma now exports the
  accumulation functions libqe did (`apply_tensor`, `apply_tensor_members`,
  `accumulate_stanza`, `rolling_window_sum`, `row_connections`,
  `aggregate_row_connections`, `finalize_row_connections`, ...). Installing
  from source needs a C++ toolchain (Rtools on Windows); requires libqe >= 0.1.9.
* The Python package pyTMA (`import pytma`, never published) is now qe-tma
  (`import tma`), with libtma compiled in (`tma.libtma`) instead of calling
  libqe's copy. Node positions come from qe-ena and generic numerics from
  qe-lib (it imported the old `pylibqe` name, so it no longer installed).
* New npm package `@qe-libs/tma-wasm` (`wasm/`): libtma compiled to
  WebAssembly, replacing libqe-wasm's accumulation functions (same
  signatures).
* New Julia package TMA.jl (`julia/`, `using TMA`) replaces LibQE.jl's
  accumulation functions, adding the weight models
  (`aggregate_row_connections`, `finalize_row_connections`). libtma is also
  published as a Conan package (`conan/`).

# tma 0.3.5

* `accumulate()` now looks each sender, receiver and mode value up in the
  tensor's own levels (the sorted levels `context_tensor()` built from the
  whole dataset). It previously re-encoded character-valued columns with
  `as.factor()` inside each unit's context, so a context containing only some
  levels mapped them to the wrong tensor slice -- for example, a context with
  only "Second" used "First"'s weight and window. Numeric columns were used
  directly as positions, so values other than 1..k (such as 0/1) pointed
  outside the tensor and could crash R. Models whose contexts contain every
  level, and models built with the default tensor, are unchanged.
* Values that are not levels of the tensor now raise a clear error.

# tma 0.3.4

* `accumulate()` gains `weight_by`, the weight model applied to each line's
  connection counts before they are summed per unit (= rENA's `weight.by`):
  `"binary"`, `"product"` (raw counts), `"sqrt"` or `"log1p"` (alias `"log"`).
  For unordered networks the weight is applied after each line is folded to
  the upper triangle; for ordered networks, to each directed cell. It defaults
  from `binary` (`"binary"` when `TRUE`, `"product"` when `FALSE`), so existing
  calls produce identical results; `"binary"` on ordered networks keeps the raw
  directed counts, as before. `model$row.connection.counts` still holds the
  raw, unweighted line counts, and `weight_by` is recorded in
  `_function.params`.
* The per-unit aggregation now uses libqe's shared
  `aggregate_row_connections()` kernel; the R package requires libqe 0.1.5 or
  newer.
* pytma 0.1.2: `TMA.fit()` gains the same `weight_by` argument and requires
  pylibqe 0.1.5 or newer.
* `contexts()` now warns when a conversation rule compares a column to `UNIT`
  with `==` (e.g. `GroupName == UNIT$GroupName`) and a unit has more than one
  value for that column. `UNIT$<col>` holds every value across the unit's rows,
  so `==` recycles and silently selects the wrong rows; use `%in%` instead.

# tma 0.3.3

* Added door smoothing helpers for lookback, EMA, and LOESS workflows.
* Added `door_accumulation()` as a convenience wrapper for the standard ETM
  `door()` plus `replace_door()` workflow.
* Added trajectory helpers for polynomial curve fitting, derivative profiles,
  curve distances, lagged distances, signed turn-lag analysis, and stability
  grids.
* Updated the R package dependency to require libqe 0.1.2 or newer for the
  shared door and trajectory kernels.

# tma 0.3.2

* improved performance of apply_tensor

# tma 0.3.1

* added exports: ATTR_NAMES, adjacency_key 
* added class as and is S3 methods for: qe.metadata, qe.data, qe.code, qe.unit, qe.horizon
* moved jsonlite to Suggests, only required in less often used tma::view()

# tma 0.3.0

* context_tensor function for controlling weights and windows across modalities
* deprecated prior implementations: windows_weights, accum_multidim, decay, simple_window

# tma 0.2.0

* Wide variety of updates, including the abiltiy to supply a windows and weighting matrix used for granular
  control of the accumulation across modalities (see `?windows_weights)

# tma 0.1.0

* Beta version - never submitted to CRAN
