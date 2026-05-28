import numpy as np
import pandas as pd
import pytest
from pytma import TMA, context_tensor


def make_df(seed=42):
    rng = np.random.default_rng(seed)
    n = 12
    return pd.DataFrame({
        "user":  [f"U{i%3}" for i in range(n)],
        "convo": [f"C{i%2}" for i in range(n)],
        "c1": rng.integers(0, 2, n).astype(float),
        "c2": rng.integers(0, 2, n).astype(float),
        "c3": rng.integers(0, 2, n).astype(float),
    })

CODES = ["c1", "c2", "c3"]


class TestTMABasicShape:
    def test_networks_shape(self):
        df = make_df()
        m = TMA().fit(df, "user", "convo", CODES)
        # 3 units, 3 connections (choose_two(3))
        assert m.networks_.shape == (3, 3)

    def test_positions_shape(self):
        df = make_df()
        m = TMA().fit(df, "user", "convo", CODES)
        assert m.positions_.shape == (3, 2)

    def test_centroids_shape(self):
        df = make_df()
        m = TMA().fit(df, "user", "convo", CODES)
        assert m.centroids_.shape == (3, 2)

    def test_fit_returns_self(self):
        df = make_df()
        m = TMA()
        assert m.fit(df, "user", "convo", CODES) is m


class TestTMAOrdered:
    def test_ordered_connection_count(self):
        df = make_df()
        m = TMA().fit(df, "user", "convo", CODES, ordered=True)
        # ordered -> p^2 = 9 connections
        assert m.networks_.shape == (3, 9)

    def test_ordered_connection_names_format(self):
        df = make_df()
        m = TMA().fit(df, "user", "convo", CODES, ordered=True)
        assert "->" in m.connection_names_[0]


class TestTMAWithTensor:
    def test_custom_tensor_runs(self):
        df = make_df()
        t = context_tensor(df, None, None, None, default_window=4.0, default_weight=1.0)
        m = TMA().fit(df, "user", "convo", CODES, tensor=t)
        assert m.networks_.shape[0] == 3

    def test_zero_weight_tensor_gives_zero_networks(self):
        df = make_df()
        t = context_tensor(df, None, None, None, default_window=5.0, default_weight=0.0)
        m = TMA().fit(df, "user", "convo", CODES, tensor=t)
        assert np.all(m.networks_ == 0)


class TestTMANorm:
    def test_invalid_norm_raises(self):
        df = make_df()
        with pytest.raises(ValueError, match="norm must be"):
            TMA().fit(df, "user", "convo", CODES, norm="bad")

    def test_skip_sphere_runs(self):
        df = make_df()
        m = TMA().fit(df, "user", "convo", CODES, norm="skip_sphere")
        assert m.normed_networks_.shape == (3, 3)


class TestTMADims:
    def test_dims_1(self):
        df = make_df()
        m = TMA().fit(df, "user", "convo", CODES, dims=1)
        assert m.positions_.shape[1] == 1
        assert m.centroids_.shape[1] == 1
