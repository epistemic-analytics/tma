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
