import numpy as np
import pandas as pd
from dataclasses import dataclass, field
from typing import Optional

@dataclass
class ContextTensor:
    """Multi-dimensional tensor of per-sender/receiver/mode weights and windows.

    The tensor array has shape (n_s1[, n_s2, ...], n_r1[, n_r2, ...], n_modes, 2)
    where the last axis is [weight, window].

    Attributes
    ----------
    array : np.ndarray
        The raw tensor values.
    sender_cols : list[str]
        Column names used as sender dimensions.
    receiver_cols : list[str]
        Column names used as receiver dimensions.
    mode_col : str | None
        Column name used as mode dimension (None if not used).
    sender_levels : dict[str, list]
        Unique sorted values per sender column (for integer encoding).
    receiver_levels : dict[str, list]
        Unique sorted values per receiver column.
    mode_levels : list
        Unique sorted mode values.
    sender_inds : list[int]
        0-based axis indices for sender dims in the tensor.
    receiver_inds : list[int]
        0-based axis indices for receiver dims.
    mode_inds : list[int]
        0-based axis indices for mode dim.
    """
    array: np.ndarray
    sender_cols: list
    receiver_cols: list
    mode_col: Optional[str]
    sender_levels: dict
    receiver_levels: dict
    mode_levels: list
    sender_inds: list
    receiver_inds: list
    mode_inds: list

    @property
    def dims(self) -> list[int]:
        """Shape of the tensor as a flat list (for apply_tensor_unit)."""
        return list(self.array.shape)

    @property
    def flat(self) -> np.ndarray:
        """Column-major (Fortran-order) flattened tensor for apply_tensor_unit."""
        return np.asarray(self.array, order='F').ravel(order='F').astype(np.float64)

    def encode_row(self, row: pd.Series) -> list[int]:
        """Convert a data row to 0-based tensor axis indices (excluding weight/window axis).

        Returns a list of ints with length = n_sender_dims + n_receiver_dims + n_mode_dims.
        Used to build context_lookup for apply_tensor_unit.
        """
        indices = []
        for col in self.sender_cols:
            indices.append(self.sender_levels[col].index(row[col]))
        for col in self.receiver_cols:
            indices.append(self.receiver_levels[col].index(row[col]))
        if self.mode_col is not None:
            indices.append(self.mode_levels.index(row[self.mode_col]))
        return indices

    def __getitem__(self, key):
        return self.array[key]

    def __setitem__(self, key, value):
        self.array[key] = value


def context_tensor(
    df: pd.DataFrame,
    sender_cols: Optional[list] = None,
    receiver_cols: Optional[list] = None,
    mode_col: Optional[str] = None,
    default_window: float = 1.0,
    default_weight: float = 1.0,
) -> ContextTensor:
    """Build a multi-dimensional tensor of weights and windows.

    Mirrors R's context_tensor(). The tensor has shape
    (n_s1[, n_s2, ...], n_r1[, n_r2, ...][, n_modes], 2)
    where the last axis is [weight=0, window=1].

    All entries are initialized to default_weight (axis 0) and
    default_window (axis 1). Modify tensor.array in-place to set
    per-combination values.

    Parameters
    ----------
    df : pd.DataFrame
    sender_cols : list[str] | None
    receiver_cols : list[str] | None
    mode_col : str | None
    default_window : float
    default_weight : float

    Returns
    -------
    ContextTensor
    """
    sender_cols  = list(sender_cols)  if sender_cols  else []
    receiver_cols = list(receiver_cols) if receiver_cols else []

    # Compute unique sorted levels per column
    sender_levels  = {c: sorted(df[c].unique().tolist()) for c in sender_cols}
    receiver_levels = {c: sorted(df[c].unique().tolist()) for c in receiver_cols}
    mode_levels    = sorted(df[mode_col].unique().tolist()) if mode_col else []

    # Build shape: [s dims..., r dims..., mode dim (if any), 2]
    shape = (
        [len(sender_levels[c])  for c in sender_cols] +
        [len(receiver_levels[c]) for c in receiver_cols] +
        ([len(mode_levels)] if mode_levels else []) +
        [2]
    )

    # Axis index bookkeeping (0-based)
    n_s = len(sender_cols)
    n_r = len(receiver_cols)
    sender_inds   = list(range(n_s))
    receiver_inds = list(range(n_s, n_s + n_r))
    mode_inds     = [n_s + n_r] if mode_levels else []

    # Initialize: weight axis = default_weight, window axis = default_window
    arr = np.empty(shape, dtype=np.float64)
    arr[..., 0] = default_weight
    arr[..., 1] = default_window

    return ContextTensor(
        array=arr,
        sender_cols=sender_cols,
        receiver_cols=receiver_cols,
        mode_col=mode_col,
        sender_levels=sender_levels,
        receiver_levels=receiver_levels,
        mode_levels=mode_levels,
        sender_inds=sender_inds,
        receiver_inds=receiver_inds,
        mode_inds=mode_inds,
    )
