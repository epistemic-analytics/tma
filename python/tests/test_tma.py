import numpy as np
import pandas as pd
import pytest
from tma import TMA, context_tensor


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

    def test_ordered_connection_names_match_r_adjacency_key(self):
        # R: colnames(adjacency_key(c("c1","c2","c3"), FALSE)) -- ground i ->
        # response j at j*n+i, labelled "i & j"
        df = make_df()
        m = TMA().fit(df, "user", "convo", CODES, ordered=True)
        assert m.connection_names_ == [
            "c1 & c1", "c2 & c1", "c3 & c1",
            "c1 & c2", "c2 & c2", "c3 & c2",
            "c1 & c3", "c2 & c3", "c3 & c3",
        ]

    def test_unordered_connection_names_match_r_adjacency_key(self):
        # R: colnames(adjacency_key(c("c1","c2","c3"), TRUE))
        m = TMA().fit(make_df(), "user", "convo", CODES)
        assert m.connection_names_ == ["c1 & c2", "c1 & c3", "c2 & c3"]


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


class TestWeightBy:
    """weight_by applies per line before the unit sum (= R tma / rENA weight.by)."""

    # Unit x: one conversation, window 3, counts > 1. Reference values are
    # rENA's legacy ena.accumulate.data(weight.by = ...) (A & B, A & C, B & C).
    # Units y and z sit in their own conversations (so x is unaffected) and
    # only exist so the rotation / projection steps of fit() have enough units.
    TOY = pd.DataFrame({
        "u": ["x", "x", "x", "y", "y", "z", "z"],
        "c": ["1", "1", "1", "2", "2", "3", "3"],
        "A": [2.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0],
        "B": [0.0, 3.0, 1.0, 1.0, 0.0, 1.0, 1.0],
        "C": [1.0, 0.0, 2.0, 0.0, 1.0, 1.0, 0.0],
    })

    def networks(self, dims=2, **kw):
        tensor = context_tensor(self.TOY, default_window=3.0)
        m = TMA().fit(self.TOY, units="u", conversations="c", codes=["A", "B", "C"],
                      tensor=tensor, dims=dims, **kw)
        return m.networks_[list(m.units_).index("x")]

    def test_matches_rena_weight_by(self):
        np.testing.assert_allclose(self.networks(weight_by="binary"),  [2, 3, 2])
        np.testing.assert_allclose(self.networks(weight_by="product"), [12, 9, 12])
        np.testing.assert_allclose(self.networks(weight_by="sqrt"),
                                   [4.73205080756888, 4.86370330515627, 4.73205080756888], rtol=1e-12)
        np.testing.assert_allclose(self.networks(weight_by="log1p"),
                                   [3.68887945411394, 3.73766961828337, 3.68887945411394], rtol=1e-12)

    def test_defaults_follow_binary_flag(self):
        np.testing.assert_array_equal(self.networks(), self.networks(weight_by="binary"))
        np.testing.assert_array_equal(self.networks(binary=False), self.networks(weight_by="product"))

    def test_invalid_weight_by_raises(self):
        with pytest.raises(ValueError):
            self.networks(weight_by="cube")
        with pytest.raises(ValueError):
            self.networks(weight_by=np.sqrt)


class TestMisses:
    """misses=True mirrors R tma's accumulate(misses = TRUE) (test-misses.R)."""

    TOY = TestWeightBy.TOY

    def fit(self, **kw):
        tensor = context_tensor(self.TOY, default_window=3.0)
        return TMA().fit(self.TOY, units="u", conversations="c", codes=["A", "B", "C"],
                         tensor=tensor, **kw)

    def misses(self, **kw):
        m = self.fit(misses=True, **kw)
        return m.miss_networks_[list(m.units_).index("x")]

    def test_unit_misses(self):
        np.testing.assert_allclose(self.misses(weight_by="product"), [3, 3, 0])
        np.testing.assert_allclose(self.misses(weight_by="binary"), [1, 2, 0])
        np.testing.assert_allclose(self.misses(ordered=True), [3, 3, 1, 0, 0, 0, 2, 0, 1])

    def test_off_by_default_and_model_unchanged(self):
        plain = self.fit()
        assert plain.miss_networks_ is None
        np.testing.assert_array_equal(self.fit(misses=True).networks_, plain.networks_)
class TestMatchesRAccumulate:
    """Unit networks equal R tma's accumulate() (weight_by = "product", window 3).

    Unit z has a single line (A and B) in its own conversation: the kernel's
    response self-term still connects A and B, so it is not a zero network.
    R reference: accumulate(contexts(toy, units_by = "u", hoo_rules =
    conversation_rules((c %in% UNIT$c))), c("A","B","C"), context_tensor(toy,
    NULL, NULL, default_window = 3, default_weight = 1), weight_by = "product").
    """

    TOY = pd.DataFrame({
        "u": ["x", "x", "x", "y", "y", "z", "w", "w"],
        "c": ["1", "1", "1", "2", "2", "3", "4", "4"],
        "A": [2.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0],
        "B": [0.0, 3.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0],
        "C": [1.0, 0.0, 2.0, 0.0, 1.0, 0.0, 1.0, 0.0],
    })

    R_UNORDERED = {"x": [12, 9, 12], "y": [2, 2, 1], "z": [1, 0, 0], "w": [2, 1, 2]}
    R_ORDERED = {  # A & A, B & A, C & A, A & B, B & B, C & B, A & C, B & C, C & C
        "x": [2, 1.5, 2, 10.5, 3, 5, 7, 7, 2],
        "y": [1, 1.5, 0.5, 0.5, 0, 0, 1.5, 1, 0],
        "z": [0, 0.5, 0, 0.5, 0, 0, 0, 0, 0],
        "w": [0, 1.5, 1, 0.5, 1, 1.5, 0, 0.5, 0],
    }

    def networks(self, ordered):
        tensor = context_tensor(self.TOY, default_window=3.0)
        m = TMA().fit(self.TOY, units="u", conversations="c", codes=["A", "B", "C"],
                      tensor=tensor, ordered=ordered, weight_by="product")
        return {u: m.networks_[i] for i, u in enumerate(m.units_)}

    def test_unordered(self):
        nets = self.networks(False)
        for u, ref in self.R_UNORDERED.items():
            np.testing.assert_allclose(nets[u], ref, err_msg=u)

    def test_ordered(self):
        nets = self.networks(True)
        for u, ref in self.R_ORDERED.items():
            np.testing.assert_allclose(nets[u], ref, err_msg=u)
