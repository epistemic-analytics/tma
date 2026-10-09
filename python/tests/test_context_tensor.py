import numpy as np
import pandas as pd
import pytest
from tma import context_tensor, ContextTensor


def make_df():
    return pd.DataFrame({
        "sender": ["A", "A", "B", "B"],
        "receiver": ["X", "Y", "X", "Y"],
        "mode": ["chat", "chat", "resource", "resource"],
        "c1": [1, 0, 1, 0],
        "c2": [0, 1, 0, 1],
    })


class TestContextTensorShape:
    def test_returns_context_tensor(self):
        df = make_df()
        t = context_tensor(df, ["sender"], ["receiver"], "mode")
        assert isinstance(t, ContextTensor)

    def test_shape_sender_receiver_mode(self):
        df = make_df()
        t = context_tensor(df, ["sender"], ["receiver"], "mode")
        # 2 senders x 2 receivers x 2 modes x 2 (weight/window)
        assert t.array.shape == (2, 2, 2, 2)

    def test_shape_no_mode(self):
        df = make_df()
        t = context_tensor(df, ["sender"], ["receiver"], None)
        assert t.array.shape == (2, 2, 2)

    def test_shape_no_sender_receiver(self):
        df = make_df()
        t = context_tensor(df, None, None, None)
        # No dims -> just (2,) for weight/window
        assert t.array.shape == (2,)

    def test_default_weight_and_window(self):
        df = make_df()
        t = context_tensor(df, ["sender"], ["receiver"], None,
                           default_window=4.0, default_weight=2.0)
        # weight axis (index 0 in last dim) = 2.0
        assert np.all(t.array[..., 0] == 2.0)
        # window axis (index 1) = 4.0
        assert np.all(t.array[..., 1] == 4.0)


class TestContextTensorHelpers:
    def test_flat_length(self):
        df = make_df()
        t = context_tensor(df, ["sender"], ["receiver"], "mode")
        assert t.flat.shape[0] == np.prod(t.array.shape)

    def test_dims_matches_shape(self):
        df = make_df()
        t = context_tensor(df, ["sender"], ["receiver"], "mode")
        assert t.dims == list(t.array.shape)

    def test_axis_indices(self):
        df = make_df()
        t = context_tensor(df, ["sender"], ["receiver"], "mode")
        assert t.sender_inds   == [0]
        assert t.receiver_inds == [1]
        assert t.mode_inds     == [2]

    def test_encode_row(self):
        df = make_df()
        t = context_tensor(df, ["sender"], ["receiver"], "mode")
        row = pd.Series({"sender": "B", "receiver": "X", "mode": "resource"})
        enc = t.encode_row(row)
        # sender B=1 (sorted: A=0, B=1), receiver X=0, mode resource=1
        assert enc == [1, 0, 1]
