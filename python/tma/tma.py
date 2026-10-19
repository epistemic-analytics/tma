"""tma.tma — High-level TMA pipeline."""
from __future__ import annotations

import numpy as np
import pandas as pd
from itertools import combinations
from typing import Optional, Union

from ena import libena                    # node positions (libena, qe-ena)
from qe import adjacency, normalization, modeling  # generic numerics (libqe, qe-lib)

from . import libtma                      # accumulation (libtma, this package)
from .context_tensor import ContextTensor, context_tensor as make_context_tensor
from .contexts import ContextModel, contexts as make_contexts


class TMA:
    """Transmodal Analysis pipeline.

    Mirrors R's tma pipeline: contexts -> accumulate (apply_tensor_unit) ->
    normalize -> position.

    Attributes (set after fit)
    ----------
    units_ : list[str]
    codes_ : list[str]
    connection_names_ : list[str]
    networks_ : np.ndarray (n_units x n_connections)
    miss_networks_ : np.ndarray (n_units x n_connections) | None
        Missed co-occurrences; only when fit with ``misses=True``.
    self_networks_ : np.ndarray (n_units x n_codes)
        Self-connections: each line's ground i x response i (the ordered
        network's diagonal, which unordered networks fold away), weighted per
        line and summed per unit. Not used for the model.
    self_miss_networks_ : np.ndarray (n_units x n_codes) | None
        Self-misses (code i in the ground, absent from the response); only
        when fit with ``misses=True``.
    normed_networks_ : np.ndarray
    positions_ : np.ndarray (n_codes x dims)
    centroids_ : np.ndarray (n_units x dims)
    weights_ : np.ndarray
    points_ : np.ndarray
    rotation_ : np.ndarray
    """

    def fit(
        self,
        data: pd.DataFrame,
        units: Union[str, list[str]],
        conversations: Union[str, list[str]],
        codes: list[str],
        tensor: Optional[ContextTensor] = None,
        time_col: Optional[str] = None,
        ordered: bool = False,
        binary: bool = True,
        weight_by: Optional[str] = None,
        misses: bool = True,
        norm: str = "sphere",
        dims: int = 2,
    ) -> "TMA":
        """Fit a TMA model.

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
            Folded (unordered) and weighted per line like the connection
            counts; stored in ``miss_networks_`` (and self-misses in
            ``self_miss_networks_``). Not used for the model. Default True;
            False skips them (less memory for many codes or long data).
        norm : {"sphere", "skip_sphere"}
        dims : int
        """
        if norm not in ("sphere", "skip_sphere"):
            raise ValueError(f"norm must be 'sphere' or 'skip_sphere', got {norm!r}")
        if weight_by is None:
            weight_by = "binary" if binary else "product"
        if not isinstance(weight_by, str):
            raise ValueError("weight_by must be one of 'binary', 'product', 'sqrt', 'log1p'")

        units_cols = [units] if isinstance(units, str) else list(units)
        n_codes = len(codes)
        n_connections = n_codes * (n_codes - 1) // 2 if not ordered else n_codes * n_codes

        # 1. Build contexts
        ctx_model = make_contexts(data, units, conversations)
        df = ctx_model.data

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
            times = np.arange(len(df), dtype=np.float64)

        # 4. Accumulate per unit using apply_tensor_unit
        unit_labels = ctx_model.unit_labels
        n_units = len(unit_labels)

        raw_networks = np.zeros((n_units, n_connections), dtype=np.float64)
        miss_networks = np.zeros((n_units, n_connections), dtype=np.float64) if misses else None
        self_networks = np.zeros((n_units, n_codes), dtype=np.float64)
        self_miss_networks = np.zeros((n_units, n_codes), dtype=np.float64) if misses else None

        tensor_flat = tensor.flat
        tensor_dims = tensor.dims
        sender_inds   = tensor.sender_inds
        receiver_inds = tensor.receiver_inds
        mode_inds     = tensor.mode_inds

        # Build context_lookup: integer matrix (n_rows x n_factor_cols) in int32
        n_factor_cols = len(sender_inds) + len(receiver_inds) + len(mode_inds)

        if n_factor_cols > 0:
            context_lookup = np.zeros((len(df), n_factor_cols), dtype=np.int32)
            for row_idx, row in df.iterrows():
                context_lookup[row_idx] = tensor.encode_row(row)
        else:
            # Default tensor path: empty context_lookup (IS_DEFAULT branch in C++)
            context_lookup = np.zeros((len(df), 0), dtype=np.int32)

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
            unit_rows_local = [vis_idx[r] for r in own_rows if r in vis_idx]

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
                ordered, weight_by, misses,
            )
            raw_networks[unit_idx] = result.connection_counts
            self_networks[unit_idx] = result.self_connection_counts
            if misses:
                miss_networks[unit_idx] = result.miss_counts
                self_miss_networks[unit_idx] = result.self_miss_counts

        # 5. Normalize. qe-lib's normalization submodule exposes these as
        # normalize_networks (per-row L2 -> unit sphere, == rENA fun_sphere_norm)
        # and scale_networks (scale by the largest row norm, == fun_skip_sphere_norm).
        if norm == "sphere":
            normed = normalization.normalize_networks(raw_networks)
        else:
            normed = normalization.scale_networks(raw_networks)

        # 6. SVD + position (same pipeline as pyENA)
        centered = modeling.center_points(normed)
        _, _, Vt = np.linalg.svd(centered, full_matrices=False)
        rotation = Vt.T[:, :dims]
        t = centered @ rotation

        non_zero = normed.sum(axis=1) != 0
        if non_zero.any():
            positions = libena.node_positions(
                np.ascontiguousarray(normed[non_zero]),
                np.ascontiguousarray(t[non_zero]),
                dims,
            )
            mean_centroids = positions.centroids.mean(axis=0)
            centroids_full = t.copy()
            centroids_full[non_zero] = positions.centroids - mean_centroids
            nodes = positions.nodes - mean_centroids
            _weights = positions.weights
            _points  = positions.points
        else:
            nodes = np.zeros((n_codes, dims))
            centroids_full = t.copy()
            _weights = np.zeros((n_units, n_codes))
            _points  = t.copy()

        # 7. Store results
        self.units_            = unit_labels
        self.codes_            = list(codes)
        # Same labels as R's adjacency_key(): unordered from libqe; ordered in
        # the kernel's layout, ground i -> response j at j*n+i, as "i & j".
        self.connection_names_ = list(adjacency.connection_names(list(codes))) if not ordered else [
            f"{codes[i]} & {codes[j]}"
            for j in range(n_codes)
            for i in range(n_codes)
        ]
        self.networks_         = raw_networks
        self.miss_networks_    = miss_networks
        self.self_networks_    = self_networks
        self.self_miss_networks_ = self_miss_networks
        self.normed_networks_  = normed
        self.positions_        = nodes
        self.centroids_        = centroids_full
        self.weights_          = _weights
        self.points_           = _points
        self.rotation_         = rotation

        return self
