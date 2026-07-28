# Design: flexible (per-modality) windows in the webtool Data View hover

Status: proposal / pre-implementation
Author: investigation captured 2026-07-28
Scope: `tma` (core), `rENA-api` (server), `rENA-webtool` (client)
Related: tma v0.3.2; `[[tma-flexible-horizon-split]]`, `[[algorithm-placement-policy]]`

## 1. Problem

In the webtool Data View, a user can click an edge of a network to activate the
view, then hover rows to see which lines **co-occur within the moving window**
for the activated unit/mean. This works for standard models with a single window
size.

TMA supports **dynamic window sizes per modality** (the window/weight matrix /
`context_tensor` approach). That per-modality windowing is **not represented in
the Data View** — the hover highlight uses one uniform window regardless of a
row's modality. So the Data View can disagree with the network it was launched
from, and it cannot show *why* a row is or isn't in a given row's window.

## 2. How the hover works today

1. Client — `DataViewCtrl` (`rENA-webtool/js/ena.controllers.js`, ~L3853) emits
   `/data/get.conversation` with only `{ settings.model.stanzaSize }` (one number).
2. Server — handler (`rENA-webtool/server.js`, ~L2675) reduces it to
   `window = stanzaSize` and calls R `rENA::ena.conversations`.
3. R — `ena.conversations(set, units, units.by, codes, conversation.by, window,
   conversation.exclude)` returns `{ conversations, unitConvs, allRows, unitRows,
   toRemove }`; the webtool's `getConversationCallback` renders `open`/`allRows`
   as the highlighted co-occurring rows.

The **network** path is fully TMA-aware — model generation
(`rENA-webtool/server.js`, ~L897–950, the `isTransmodal` block) builds
`tma_array`, `mode_column`, `hoo`, ground/response cols, time cols. The
**Data View** path was never given the same treatment.

## 3. Root cause

Three layers, bottom one is the blocker:

| Layer | Today | Needed |
|---|---|---|
| Client `DataViewCtrl` | sends only `stanzaSize` | send TMA context |
| Server `get.conversation` | builds `{ window }` | build/route TMA params |
| R conversation fn | single `window`, no modality | per-modality windows |

Two specific findings in `tma`:

- **`tma.conversations` predates the window/weight matrix.** Its core is
  `winUse = ifelse(is.infinite(window), thisRowInConv, window)` — a uniform
  scalar. It adds TMA-object plumbing (`x$model$raw.input`, `id_col`) and returns
  the shape the webtool wants, but it does **not** use modality/decay. Confirmed:
  `windows_weights` is `.Deprecated("context_tensor", "tma v0.2.0")`, i.e. the
  matrix approach superseded the old windowing after `tma.conversations` was
  written.
- **`_function.params` does not retain the windowing config.** On a live
  `contexts() -> accumulate()` object (`test_mockdata`), `_function.params` =
  `{ units, units.by }` only. Its intent is "what was used to build the model, so
  it can be recreated" — but for TMA it is incomplete: no `tensor`, no
  `mode_column`, no hoo. So even handed the accumulated `x`, a conversation
  function can't recover the per-modality windows.

## 4. What the accumulated object retains (verified live)

```
acc
├─ _function.params  = { units, units.by }                 # NO windowing config
├─ model
│   ├─ model.type          = "TMA"
│   ├─ raw.input           (rows)
│   ├─ contexts            # hoo splits, per unit ("User1::FirstHalf", ...)   RETAINED
│   │     each context[[unit]] is a data.table of horizon rows, with attrs:
│   │       attr "tma.unit"       (unit def)
│   │       attr "tma.unit_rows"  (the unit's own coded row indices)
│   ├─ row.connection.counts   # per-row CODE-PAIR counts (QEID, ..., "A & B"…) RETAINED
│   └─ unit.labels
├─ rotation, connection.counts, meta.data
```

- hoo / horizon membership: **retained** in `model$contexts`.
- window/weight tensor + `mode_column`: **not retained** — `accumulate()` builds
  `context_tensor(context_model$model$raw.input)` on the fly and discards it. No
  3–4D tensor array survives anywhere on the object.
- `row.connection.counts` is aggregated to **code** connections per row, so it is
  not a row→row window-membership map; it cannot by itself drive the hover.

## 5. Proposed design

Keep the windowing algorithm in the algorithm layer (`[[algorithm-placement-policy]]`);
the server/client only pass params and render. Four layers:

### 5a. `tma` — persist the windowing config into `_function.params`

At `accumulate()` time, complete the recreate-the-model record (value beyond the
Data View: true reproducibility):

```r
x$`_function.params` <- modifyList(x$`_function.params`, list(
  codes           = codes,
  conversation.by = conversation.by,
  mode_column     = mode.column,
  tensor          = tensor,        # the RESOLVED context_tensor actually used (incl. user overrides)
  time_column     = time.column,
  ordered         = ordered,
  binary          = binary,
  hoo_rules       = hoo_rules      # optional: lets contexts be recomputed, not just read
))
```

Minimum the Data View needs: `tensor`, `mode_column`, `codes`, `conversation.by`
(hoo is already in `model$contexts`).

### 5b. `libqe` + `tma` — new `tma.conversations2` (leave `tma.conversations` untouched)

The window application is already in C++: `accumulate()` -> `apply_tensor()` ->
`.Call("_libqe_apply_tensor", ...)`. Per `[[algorithm-placement-policy]]`, the
per-reference-row window membership must be emitted **from libqe**, not
reimplemented in R.

- **libqe** — extend `apply_tensor` to optionally return, alongside
  `row_connection_counts`, the row-level membership: for each reference/receiver
  row, the sender rows within its modality window that co-occur, each with
  `distance` and the admitting mode. This is the raw `members` data and it
  guarantees the Data View matches the network exactly (same code path).
- **tma** — `tma.conversations2` is a thin wrapper: run the (extended)
  apply_tensor over the unit's contexts and reshape the membership into
  `rowWindows` (plus the back-compatible `{conversations, unitConvs, allRows,
  unitRows, toRemove}`).

Do NOT ship a pure-R re-walk of the window as the production path (it would drift
from libqe). A throwaway R walk is fine only to prototype the `rowWindows` payload
and validate the client binding (see `prototype/rowwindows-proto.R`).

Existing users may depend on `tma.conversations`; add a v2 rather than change it.

#### libqe change — concrete (capture-and-surface, not a new algorithm)

The membership is **already computed** in the hot loop and discarded. Locations:
- core `qe::apply_tensor_unit` — `libqe/R/inst/include/libqe/accumulation.hpp:379`
- Rcpp binding `apply_tensor` — `libqe/R/src/libqe_rcpp.cpp:414`

In `apply_tensor_unit`, for each response (reference) row `ri`, the loop at
accumulation.hpp:424–443 walks ground rows backward and tests membership:

```cpp
if (times[gr] + row_win > response_time) {   // line 435 — the window test
    gri_v.push_back(gr);                       // line 436 — in-window ground rows (== members)
    ...                                         // row_win (431–433) = resolved per-modality window
}                                               // grw_v = per-member weight
```

`gri_v` is exactly the per-reference-row membership; it is used for the weighted
ground sum then thrown away. The returned `TensorNetworks` (accumulation.hpp:343)
carries only aggregated `connection_counts` + `row_connection_counts`.

Edits (all additive, flag-gated):
1. `TensorNetworks` struct (accumulation.hpp:343): add
   `std::vector<std::vector<int>> row_window_members;` (the `gri_v` per response
   row) and a parallel `std::vector<std::vector<double>> row_window_wins;`
   (the `row_win` per member).
2. `apply_tensor_unit` (~line 445): `push_back` the already-computed `gri_v` /
   `row_win` into those fields. One line per response row; no math change. Gate
   with a `bool return_members = false` param so it is zero-cost when off.
3. Rcpp `apply_tensor` (libqe_rcpp.cpp:427): add the param; when set, add
   `_("row_window_members") = ...` to the returned List (nested vectors → R list
   of integer vectors, 0->1-based; order matches `unit_rows`).
4. Regenerate + rebuild: `Rcpp::compileAttributes()` (updates `RcppExports.*`),
   reinstall libqe, cut a libqe release.

Notes:
- The window test is time-based (`times[gr] + row_win`), so it covers both line-
  and time-based windows natively; `rowWindows.distance` reflects whichever.
- Capturing `row_win` per member gives the exact admitting window (more precise
  than a single `windowByMode`) — the richer UI can use it directly.
- `admittedBy` = the ground (sender) row's modality; the receiver dims are
  overridden to the response row's values at line 429 before the window lookup.
- Risk is low: additive struct field + flag-gated return + no accumulation-math
  change. Cost is the Rcpp conversion + libqe build/release cycle.

```r
tma.conversations2 <- function(x, units, units.by = NULL, codes,
                               conversation.by, conversation.exclude = c(),
                               id_col = "QEID") {
  fp      <- x$`_function.params`
  tensor  <- fp$tensor            # per-(senderMode -> receiverMode) window/weight
  modeCol <- fp$mode_column
  # candidate rows come from the RETAINED horizon, not raw slicing:
  #   ctx      <- x$model$contexts[[ unitKey ]]
  #   unitRows <- attr(ctx, "tma.unit_rows")
  # for each coded reference row r (modality m_r) in unitRows:
  #   for each earlier row r' (modality m_r') in ctx:
  #     W <- tensor[m_r', m_r, , "window"]           # per-modality window back (was scalar)
  #     inWindow <- (pos(r) - pos(r')) <= W
  #     coOccurs <- shares a code with r
  # collect r' into allRows when inWindow && coOccurs
  list(
    # unchanged, back-compatible:
    conversations = ..., unitConvs = ..., allRows = ...,
    unitRows = ..., convRows = ..., toRemove = ...,
    # NEW, additive — drives the window-span UI:
    rowWindows = ...
  )
}
```

Prefer calling the **same** window-application primitive `accumulate_contexts` /
`context_tensor` use, so hover and network agree exactly rather than
re-deriving the test.

### 5c. `rENA-api` server — route transmodal models

In `get.conversation` (`server.js` ~L2675): when the model is transmodal, call
`tma.conversations2` with the TMA-accumulated `x` + `id_col` instead of
`ena.conversations`. Windowing comes off `x`, so few new params. Standard models
keep calling `ena.conversations`.

### 5d. `rENA-webtool` client — render the richer window-span

On hover, consume `rowWindows`:
- modality made visibly distinct (color chip / left border) via
  `ENAmodel.model.stanzaModality`;
- dashed **window-edge** markers, one per modality, at each modality's reach;
- co-occurring rows highlighted, **tinted by modality**;
- in-window-but-no-code rows shown faint;
- a small legend (modality -> color, window size).

Return shape stays back-compatible; the client derives per-row modality from the
raw row + `stanzaModality`, and uses `rowWindows` for the reach/why.

## 6. `rowWindows` return shape (additive)

```r
rowWindows = list(
  "<refRowId>" = list(
    refMode      = "chat",
    windowByMode = list(chat = 2, resource = 5),     # -> the dashed window edges
    members = list(
      list(row=<id>, mode="resource", distance=5, admittedBy="resource", coOccurs=TRUE),  # tinted highlight
      list(row=<id>, mode="chat",     distance=2, admittedBy="chat",     coOccurs=TRUE),
      list(row=<id>, mode="resource", distance=3, admittedBy="resource", coOccurs=FALSE)   # faint
    )
  )
)
```

Field -> UI mapping: `windowByMode` -> edges; `mode` -> tint; `admittedBy` ->
"in <modality> window" label; `coOccurs` -> highlight vs faint. Existing
`{conversations, unitConvs, allRows, unitRows, toRemove}` unchanged.

## 7. UI legibility requirement

The visualization is a first-class part of this work, not a follow-on: without a
visible modality distinction, per-modality windows silently highlight different
rows with no explanation. The client already has the inputs (`stanzaModality` +
each row's modality value), so the minimum (modality indicator) needs no new
server data; the richer window-span reach needs `rowWindows`.

## 8. Open questions / verification before/while coding

1. Confirm the webtool's **custom** per-modality windows (the `tma_array` at
   `server.js` ~L918) flow into the tensor `accumulate()` receives, so 5a
   persists the *resolved* tensor (with overrides), not the default one.
2. Confirm `tma.conversations2.members` / `coOccurs` for a known unit+edge match
   the network's actual in-window connections (cross-check `row.connection.counts`).
3. Decide `id_col` for the webtool path (accumulated objects expose `QEID`).
4. Time-based windows (`time_column` / duration): make sure `distance` in
   `rowWindows` is expressed correctly (lines vs time) for the UI.

## 9. Rollout / compatibility

- `tma.conversations` is left in place; `tma.conversations2` is additive.
- `_function.params` additions are backward-compatible (extra keys).
- Server routes only transmodal models to the new function; standard models
  unchanged.
- Client return shape unchanged for standard models; `rowWindows` is optional and
  only present for transmodal.

## 10. Prototype status + first implementation step

Verified so far (against `tma::test_mockdata`):
- `context_tensor(raw.input, mode_column = "modality")` yields a clean
  `[modes x {weight,window}]` array indexable by modality name
  (`ten["chat","window"]`).
- Setting chat=2 vs resource=6 and accumulating changes
  `row.connection.counts` vs a uniform tensor (totals 39 -> 54): the per-modality
  tensor genuinely drives different windows end-to-end. The Data View must mirror
  this.
- The window application is `libqe::apply_tensor` (C++); `accumulate` consumes
  only its `row_connection_counts`.

First implementation step:
1. libqe — add a membership return to `apply_tensor` (behind a flag).
2. tma — 5a (persist tensor/mode into `_function.params`) + `tma.conversations2`
   as a thin reshaper over the extended apply_tensor.
3. Assert `tma.conversations2` co-occurrences aggregate to `row.connection.counts`
   for a known unit+edge (self-consistency).
A pure-R prototype of `rowWindows` may be built first purely to unblock client UI
work, clearly marked throwaway.
