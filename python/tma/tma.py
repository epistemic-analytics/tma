"""tma.tma — TMA accumulation (= R tma's accumulate()).

qe-tma accumulates; modeling (normalization, rotation, node positions) is
qe-ena's, as in R, where rENA models tma's accumulation:

    from tma import accumulate, context_tensor
    from ena import ENA

    accum = accumulate(data, "unit", "conversation", CODES,
                       tensor=context_tensor(data, default_window=4))
    model = ENA().fit(accum)
"""
from __future__ import annotations

from typing import List, Optional, Union

import numpy as np
import pandas as pd

from qe import adjacency                  # adjacency layout (libqe, qe-lib)

from . import libtma                      # accumulation (libtma, this package)
from .context_tensor import ContextTensor, context_tensor as make_context_tensor
from .contexts import contexts as make_contexts


class TMAAccumulation:
    """Result of :func:`accumulate`: each unit's networks, not modeled.

    Field names follow R tma's ``accumulate()`` result and qe-ena's
    ``ENAAccumulation``, so ``ena.ENA().fit()`` models either.

    Attributes
    ----------
    connection_counts_ : np.ndarray (n_units x n_connections)
        Weighted connection counts summed per unit (= R ``connection.counts``).
        Columns follow ``adjacency_key_``: ordered p^2 (ground i -> response j
        at j*p+i); unordered choose(p, 2), or p(p+1)/2 with
        ``self_connections`` (A & A, A & B, B & B, A & C, ...).
    self_connection_counts_ : np.ndarray (n_units x n_codes)
        Each code's self-connection, ground i x response i (= R
        ``self.connection.counts``), kept whether or not it is in the columns.
    miss_counts_, self_miss_counts_ : np.ndarray | None
        Missed co-occurrences (= R ``miss.counts`` / ``self.miss.counts``);
        None when ``misses=False``.
    row_connection_counts_, row_self_connection_counts_,
    row_miss_counts_, row_self_miss_counts_ : np.ndarray | None
        The same per line, raw (= R ``model$row.*``): one row per data line,
        zero for lines that are no unit's own.
    unit_labels_ : list[str]
    codes_ : list[str]
    connection_names_ : list[str]
        "i & j" per column (= R ``adjacency_key()`` pasted).
    adjacency_key_ : list[list[str]]
        [[code_i, code_j], ...] per column (= R ``rotation$adjacency.key``).
    meta : pd.DataFrame
        One row per unit (unit label as index): the first line's non-code
        columns (= R ``meta.data``).
    ordered_ : bool
    self_connections_ : bool
        True when the unordered columns include the self-connections.
    weight_by_ : str
    """

    def __init__(self, **fields) -> None:
        for name, value in fields.items():
            setattr(self, name, value)

    def __repr__(self) -> str:
        kind = "ordered" if self.ordered_ else "unordered"
        return (f"<TMAAccumulation {len(self.unit_labels_)} units x "
                f"{len(self.connection_names_)} connections ({kind})>")


def _connection_key(codes: List[str], ordered: bool, diagonal: bool) -> List[List[str]]:
    n = len(codes)
    if ordered:
        # The kernel's layout: ground i -> response j at j*n+i.
        return [[codes[i], codes[j]] for j in range(n) for i in range(n)]
    names = adjacency.connection_names(list(codes), diagonal)
    return [name.split(" & ", 1) for name in names]


def accumulate(
    data: pd.DataFrame,
    units: Union[str, List[str]],
    conversations: Union[str, List[str]],
    codes: List[str],
    tensor: Optional[ContextTensor] = None,
    time_col: Optional[str] = None,
    ordered: bool = False,
    binary: bool = True,
    weight_by: Optional[str] = None,
    misses: bool = True,
    self_connections: bool = False,
) -> TMAAccumulation:
    """Accumulate each unit's network (= R tma's ``accumulate()``).

    Parameters
    ----------
    data : pd.DataFrame
    units : str | list[str]
        Column(s) identifying units of analysis.
    conversations : str | list[str]
        Column(s) segmenting conversations (context scope).
    codes : list[str]
        Code column names.
    tensor : ContextTensor | None
        Weight/window tensor. If None, builds a default tensor with
        window=1, weight=1 (equivalent to a single-step window).
    time_col : str | None
        Timestamp column. If None, uses row order as integer timestamps.
    ordered : bool
        True = directed/ONA adjacency; False = undirected/ENA.
    binary : bool
        Binarize each line's connection counts before the per-unit sum
        (unordered only; ordered counts are summed raw). Ignored when
        ``weight_by`` is given.
    weight_by : {"binary", "product", "sqrt", "log1p"} | None
        Weight model applied to each line's connection counts before they
        are summed per unit (= R tma's ``weight_by`` / rENA's ``weight.by``):
        "binary" (unordered: positive counts become 1; ordered: raw
        directed counts), "product" (raw counts), "sqrt", "log1p" (alias
        "log"). Defaults to "binary" when ``binary`` else "product".
    misses : bool
        Also accumulate missed co-occurrences (= R tma's ``misses``): for
        each response line, the window-weighted ground count of code i when
        code j is absent from the response, with no response self-term.
        Folded (unordered) and weighted per line like the connection counts.
        Default True; False skips them (less memory for many codes or long
        data).
    self_connections : bool
        Unordered only (= R tma's ``self_connections``): also include each
        code's self-connection as a connection column (A & A, A & B, B & B,
        ...), so a model built on the counts includes them. Ordered networks
        always carry the diagonal. ``self_connection_counts_`` is kept
        either way. Default False.

    Returns
    -------
    TMAAccumulation
    """
    if weight_by is None:
        weight_by = "binary" if binary else "product"
    if not isinstance(weight_by, str):
        raise ValueError("weight_by must be one of 'binary', 'product', 'sqrt', 'log1p'")

    codes = list(codes)
    n_codes = len(codes)
    diagonal = bool(self_connections) and not ordered
    n_connections = n_codes * n_codes if ordered else adjacency.n_connections(n_codes, diagonal)

    # 1. Build contexts
    ctx_model = make_contexts(data, units, conversations)
    df = ctx_model.data
    n_rows = len(df)

    # 2. Build tensor if not provided
    # Default tensor: dims=[2], array=[weight, window] — triggers IS_DEFAULT path in C++
    if tensor is None:
        tensor = make_context_tensor(
            df,
            sender_cols=None,
            receiver_cols=None,
            mode_col=None,
            default_window=1.0,
            default_weight=1.0,
        )

    # 3. Build per-row integer timestamp vector
    if time_col is not None:
        times = df[time_col].astype(float).values
    else:
        times = np.arange(n_rows, dtype=np.float64)

    # 4. Accumulate per unit
    unit_labels = ctx_model.unit_labels
    n_units = len(unit_labels)

    def table(n, width, keep=True):
        return np.zeros((n, width), dtype=np.float64) if keep else None

    connection_counts = table(n_units, n_connections)
    self_counts = table(n_units, n_codes)
    miss_counts = table(n_units, n_connections, misses)
    self_miss_counts = table(n_units, n_codes, misses)
    row_connection_counts = table(n_rows, n_connections)
    row_self_counts = table(n_rows, n_codes)
    row_miss_counts = table(n_rows, n_connections, misses)
    row_self_miss_counts = table(n_rows, n_codes, misses)

    tensor_flat = tensor.flat
    tensor_dims = tensor.dims
    sender_inds   = tensor.sender_inds
    receiver_inds = tensor.receiver_inds
    mode_inds     = tensor.mode_inds

    # Build context_lookup: integer matrix (n_rows x n_factor_cols) in int32
    n_factor_cols = len(sender_inds) + len(receiver_inds) + len(mode_inds)

    if n_factor_cols > 0:
        context_lookup = np.zeros((n_rows, n_factor_cols), dtype=np.int32)
        for row_idx, row in df.iterrows():
            context_lookup[row_idx] = tensor.encode_row(row)
    else:
        # Default tensor path: empty context_lookup (IS_DEFAULT branch in C++)
        context_lookup = np.zeros((n_rows, 0), dtype=np.int32)

    for unit_idx, unit_label in enumerate(unit_labels):
        ctx_rows = ctx_model.contexts_map[unit_label]
        own_rows = ctx_model.unit_own_rows[unit_label]

        if not own_rows:
            continue

        # context: all rows up to and including last unit row, within ctx_rows
        last_unit_row = max(own_rows)
        visible_rows = [r for r in ctx_rows if r <= last_unit_row]

        # Remap own_rows to indices within visible_rows
        vis_idx = {r: i for i, r in enumerate(visible_rows)}
        own_visible = [r for r in own_rows if r in vis_idx]
        unit_rows_local = [vis_idx[r] for r in own_visible]

        if not unit_rows_local:
            continue

        # copy=True: under pandas copy-on-write a single-row selection is a
        # read-only view, which the extension's (writable) ndarray rejects.
        codes_mat = np.ascontiguousarray(
            df.iloc[visible_rows][codes].to_numpy(dtype=np.float64, copy=True)
        )
        times_sub = np.ascontiguousarray(times[visible_rows])
        ctx_lookup_sub = np.ascontiguousarray(context_lookup[visible_rows])

        # One libtma call per unit, as tma's R accumulate(): fold
        # (unordered), per-line weight model, unit sum and the
        # self-connection diagonal, for hits and misses.
        result = libtma.accumulate_unit_networks(
            tensor_flat, tensor_dims,
            sender_inds, receiver_inds, mode_inds,
            ctx_lookup_sub, unit_rows_local,
            codes_mat, times_sub,
            ordered, weight_by, misses, diagonal,
        )
        connection_counts[unit_idx] = result.connection_counts
        self_counts[unit_idx] = result.self_connection_counts
        row_connection_counts[own_visible] = result.row_connection_counts
        row_self_counts[own_visible] = result.row_self_connection_counts
        if misses:
            miss_counts[unit_idx] = result.miss_counts
            self_miss_counts[unit_idx] = result.self_miss_counts
            row_miss_counts[own_visible] = result.row_miss_counts
            row_self_miss_counts[own_visible] = result.row_self_miss_counts

    # 5. Labels and unit metadata
    key = _connection_key(codes, ordered, diagonal)
    meta_cols = [c for c in df.columns if c not in codes and not c.startswith("_")]
    first_rows = [ctx_model.unit_own_rows[u][0] if ctx_model.unit_own_rows[u] else None
                  for u in unit_labels]
    meta = pd.DataFrame(
        [df.loc[r, meta_cols] if r is not None else pd.Series(index=meta_cols, dtype=object)
         for r in first_rows],
        index=pd.Index(unit_labels, name="unit"),
        columns=meta_cols,
    )

    return TMAAccumulation(
        connection_counts_=connection_counts,
        self_connection_counts_=self_counts,
        miss_counts_=miss_counts,
        self_miss_counts_=self_miss_counts,
        row_connection_counts_=row_connection_counts,
        row_self_connection_counts_=row_self_counts,
        row_miss_counts_=row_miss_counts,
        row_self_miss_counts_=row_self_miss_counts,
        unit_labels_=list(unit_labels),
        codes_=codes,
        connection_names_=[f"{a} & {b}" for a, b in key],
        adjacency_key_=key,
        meta=meta,
        ordered_=bool(ordered),
        self_connections_=diagonal,
        weight_by_=weight_by,
    )
