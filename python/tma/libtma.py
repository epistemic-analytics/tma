"""
tma.libtma — libtma, the accumulation C++ layer (stanza windows, context
tensors, row connections, weight models).

The C++ lives in this repository (``inst/include/libtma``); ``tma._libtma`` is
its nanobind extension.  These functions moved here from qe-lib's
``qe.accumulation`` (``import qe``) in libqe's phase 4b split; signatures and
results are unchanged.

The extension returns plain dicts so it can be loaded alongside other
products' extensions; the result classes below restore attribute access
(``result.row_connection_counts``) and the reprs qe-lib's classes had.

All matrix inputs/outputs are numpy float64 arrays.
"""
from __future__ import annotations

from typing import NamedTuple, Optional

import numpy as np

from . import _libtma

__all__ = [
    "UnitNetworks",
    "TensorNetworks",
    "connection_matrix",
    "accumulate_stanza",
    "row_connections",
    "rolling_window_sum",
    "flat_index",
    "accumulate_unit",
    "accumulate_unit_with_rows",
    "apply_tensor_unit",
    "aggregate_row_connections",
    "finalize_row_connections",
    "finalize_self_connections",
    "aggregate_self_connections",
    "UnitAccumulation",
    "accumulate_unit_networks",
]


class UnitNetworks(NamedTuple):
    """Result of accumulate_unit_with_rows.

    networks     : ndarray 1-D — flat connection vector (p^2 or choose_two(p))
    row_networks : ndarray 2-D — per-response-row p^2 connection matrix
    """
    networks: np.ndarray
    row_networks: np.ndarray

    def __repr__(self) -> str:
        r = self.row_networks.shape
        return (f"<UnitNetworks networks len={self.networks.shape[0]} "
                f"row_networks={r[0]}x{r[1]}>")


class TensorNetworks(NamedTuple):
    """Result of apply_tensor_unit.

    connection_counts     : ndarray 1-D — unit-level flat vector (p^2)
    row_connection_counts : ndarray 2-D — per-response-row p^2 matrix
    miss_counts           : ndarray 1-D | None — unit-level missed co-occurrences
                            (p^2); only with misses=True
    row_miss_counts       : ndarray 2-D | None — per-response-row p^2 missed
                            co-occurrences; only with misses=True
    """
    connection_counts: np.ndarray
    row_connection_counts: np.ndarray
    miss_counts: Optional[np.ndarray] = None
    row_miss_counts: Optional[np.ndarray] = None

    def __repr__(self) -> str:
        r = self.row_connection_counts.shape
        return (f"<TensorNetworks connection_counts len={self.connection_counts.shape[0]} "
                f"row_connection_counts={r[0]}x{r[1]}>")


class UnitAccumulation(NamedTuple):
    """Result of accumulate_unit_networks: one unit's finished networks.

    connection_counts          : ndarray 1-D — p^2 (ordered) or choose(p,2), weighted
    row_connection_counts      : ndarray 2-D — raw per line (folded when unordered)
    self_connection_counts     : ndarray 1-D — p, weighted
    row_self_connection_counts : ndarray 2-D — raw per line (n_lines x p)
    miss_counts, row_miss_counts, self_miss_counts, row_self_miss_counts :
                                 the same for missed co-occurrences, or None
                                 when misses=False
    """
    connection_counts: np.ndarray
    row_connection_counts: np.ndarray
    self_connection_counts: np.ndarray
    row_self_connection_counts: np.ndarray
    miss_counts: Optional[np.ndarray] = None
    row_miss_counts: Optional[np.ndarray] = None
    self_miss_counts: Optional[np.ndarray] = None
    row_self_miss_counts: Optional[np.ndarray] = None


def _doc(fn):
    """Take the docstring from the extension function of the same name."""
    fn.__doc__ = getattr(_libtma, fn.__name__).__doc__
    return fn


@_doc
def connection_matrix(ground, response, response_weight=1.0, ordered=True):
    return _libtma.connection_matrix(ground, response, response_weight, ordered)


@_doc
def accumulate_stanza(codes, window_back=1, window_forward=0, binary=True, ordered=False):
    return _libtma.accumulate_stanza(codes, window_back, window_forward, binary, ordered)


@_doc
def row_connections(codes, binary=True):
    return _libtma.row_connections(codes, binary)


@_doc
def rolling_window_sum(codes, window_size=1):
    return _libtma.rolling_window_sum(codes, window_size)


@_doc
def flat_index(indices, dims):
    return _libtma.flat_index(indices, dims)


@_doc
def accumulate_unit(codes, unit_rows, decay_fn, ordered=False):
    return _libtma.accumulate_unit(codes, unit_rows, decay_fn, ordered)


@_doc
def accumulate_unit_with_rows(codes, unit_rows, decay_fn, ordered=False) -> UnitNetworks:
    return UnitNetworks(**_libtma.accumulate_unit_with_rows(codes, unit_rows, decay_fn, ordered))


@_doc
def apply_tensor_unit(tensor, dims, dims_sender, dims_receiver, dims_mode,
                      context_lookup, unit_rows, codes, times, ordered=True,
                      misses=False) -> TensorNetworks:
    return TensorNetworks(**_libtma.apply_tensor_unit(
        tensor, dims, dims_sender, dims_receiver, dims_mode,
        context_lookup, unit_rows, codes, times, ordered, misses))


@_doc
def aggregate_row_connections(row_conn, n_codes, ordered=False, weight="binary"):
    return _libtma.aggregate_row_connections(row_conn, n_codes, ordered, weight)


@_doc
def finalize_row_connections(row_conn, n_codes, ordered=False, weight="binary"):
    return _libtma.finalize_row_connections(row_conn, n_codes, ordered, weight)


@_doc
def accumulate_unit_networks(tensor, dims, dims_sender, dims_receiver, dims_mode,
                             context_lookup, unit_rows, codes, times, ordered=False,
                             weight="binary", misses=True) -> UnitAccumulation:
    return UnitAccumulation(**_libtma.accumulate_unit_networks(
        tensor, dims, dims_sender, dims_receiver, dims_mode,
        context_lookup, unit_rows, codes, times, ordered, weight, misses))


@_doc
def finalize_self_connections(row_conn, n_codes, ordered=False, weight="binary"):
    return _libtma.finalize_self_connections(row_conn, n_codes, ordered, weight)


@_doc
def aggregate_self_connections(row_conn, n_codes, ordered=False, weight="binary"):
    return _libtma.aggregate_self_connections(row_conn, n_codes, ordered, weight)
