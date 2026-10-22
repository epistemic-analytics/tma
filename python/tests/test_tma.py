import numpy as np
import pandas as pd
import pytest
import tma
from tma import TMAAccumulation, accumulate, context_tensor


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


class TestAccumulateOnly:
    """qe-tma accumulates; modeling is qe-ena's (as R: rENA models tma)."""

    def test_returns_accumulation_without_a_model(self):
        a = accumulate(make_df(), "user", "convo", CODES)
        assert isinstance(a, TMAAccumulation)
        for model_field in ("rotation_", "positions_", "points_", "centroids_", "line_weights_"):
            assert not hasattr(a, model_field)
        assert not hasattr(tma, "TMA")

    def test_does_not_import_qe_ena(self, tmp_path):
        # A fresh interpreter, outside the source tree (which has no
        # compiled extension), importing the installed package.
        import subprocess, sys
        out = subprocess.run([sys.executable, "-c", "import sys, tma; print('ena' in sys.modules)"],
                             capture_output=True, text=True, check=True, cwd=tmp_path)
        assert out.stdout.strip() == "False"


class TestShape:
    def test_networks_shape(self):
        a = accumulate(make_df(), "user", "convo", CODES)
        # 3 units, 3 connections (choose_two(3))
        assert a.connection_counts_.shape == (3, 3)
        assert len(a.unit_labels_) == 3
        assert a.ordered_ is False and a.self_connections_ is False

    def test_meta_one_row_per_unit(self):
        df = make_df()
        a = accumulate(df, "user", "convo", CODES)
        assert list(a.meta.index) == a.unit_labels_
        assert "user" in a.meta.columns and "convo" in a.meta.columns
        assert not any(c in a.meta.columns for c in CODES)
        assert list(a.meta["user"]) == a.unit_labels_

    def test_row_tables_sum_to_unit_counts_for_product(self):
        df = make_df()
        t = context_tensor(df, default_window=3.0)
        a = accumulate(df, "user", "convo", CODES, tensor=t, weight_by="product")
        assert a.row_connection_counts_.shape == (len(df), 3)
        units = df["user"].tolist()
        for i, u in enumerate(a.unit_labels_):
            rows = [r for r, v in enumerate(units) if v == u]
            np.testing.assert_allclose(a.row_connection_counts_[rows].sum(axis=0),
                                       a.connection_counts_[i], err_msg=u)


class TestOrdered:
    def test_ordered_connection_count(self):
        a = accumulate(make_df(), "user", "convo", CODES, ordered=True)
        # ordered -> p^2 = 9 connections
        assert a.connection_counts_.shape == (3, 9)
        assert a.ordered_ is True

    def test_ordered_connection_names_match_r_adjacency_key(self):
        # R: colnames(adjacency_key(c("c1","c2","c3"), FALSE)) -- ground i ->
        # response j at j*n+i, labelled "i & j"
        a = accumulate(make_df(), "user", "convo", CODES, ordered=True)
        assert a.connection_names_ == [
            "c1 & c1", "c2 & c1", "c3 & c1",
            "c1 & c2", "c2 & c2", "c3 & c2",
            "c1 & c3", "c2 & c3", "c3 & c3",
        ]
        assert a.adjacency_key_[1] == ["c2", "c1"]

    def test_unordered_connection_names_match_r_adjacency_key(self):
        # R: colnames(adjacency_key(c("c1","c2","c3"), TRUE))
        a = accumulate(make_df(), "user", "convo", CODES)
        assert a.connection_names_ == ["c1 & c2", "c1 & c3", "c2 & c3"]
        assert a.adjacency_key_ == [["c1", "c2"], ["c1", "c3"], ["c2", "c3"]]


class TestWithTensor:
    def test_custom_tensor_runs(self):
        df = make_df()
        t = context_tensor(df, None, None, None, default_window=4.0, default_weight=1.0)
        a = accumulate(df, "user", "convo", CODES, tensor=t)
        assert a.connection_counts_.shape[0] == 3

    def test_zero_weight_tensor_gives_zero_networks(self):
        df = make_df()
        t = context_tensor(df, None, None, None, default_window=5.0, default_weight=0.0)
        a = accumulate(df, "user", "convo", CODES, tensor=t)
        assert np.all(a.connection_counts_ == 0)


class TestWeightBy:
    """weight_by applies per line before the unit sum (= R tma / rENA weight.by)."""

    # Unit x: one conversation, window 3, counts > 1. Reference values are
    # rENA's legacy ena.accumulate.data(weight.by = ...) (A & B, A & C, B & C).
    TOY = pd.DataFrame({
        "u": ["x", "x", "x", "y", "y", "z", "z"],
        "c": ["1", "1", "1", "2", "2", "3", "3"],
        "A": [2.0, 1.0, 0.0, 1.0, 1.0, 0.0, 1.0],
        "B": [0.0, 3.0, 1.0, 1.0, 0.0, 1.0, 1.0],
        "C": [1.0, 0.0, 2.0, 0.0, 1.0, 1.0, 0.0],
    })

    def networks(self, **kw):
        tensor = context_tensor(self.TOY, default_window=3.0)
        a = accumulate(self.TOY, units="u", conversations="c", codes=["A", "B", "C"],
                       tensor=tensor, **kw)
        return a.connection_counts_[a.unit_labels_.index("x")]

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

    def accum(self, **kw):
        tensor = context_tensor(self.TOY, default_window=3.0)
        return accumulate(self.TOY, units="u", conversations="c", codes=["A", "B", "C"],
                          tensor=tensor, **kw)

    def misses(self, **kw):
        a = self.accum(misses=True, **kw)
        return a.miss_counts_[a.unit_labels_.index("x")]

    def test_unit_misses(self):
        np.testing.assert_allclose(self.misses(weight_by="product"), [3, 3, 0])
        np.testing.assert_allclose(self.misses(weight_by="binary"), [1, 2, 0])
        np.testing.assert_allclose(self.misses(ordered=True), [3, 3, 1, 0, 0, 0, 2, 0, 1])

    def test_on_by_default_off_with_false_and_counts_unchanged(self):
        plain = self.accum(misses=False)
        assert plain.miss_counts_ is None and plain.row_miss_counts_ is None
        assert self.accum().miss_counts_ is not None   # on by default
        np.testing.assert_array_equal(self.accum(misses=True).connection_counts_,
                                      plain.connection_counts_)


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
        a = accumulate(self.TOY, units="u", conversations="c", codes=["A", "B", "C"],
                       tensor=tensor, ordered=ordered, weight_by="product")
        return {u: a.connection_counts_[i] for i, u in enumerate(a.unit_labels_)}

    def test_unordered(self):
        nets = self.networks(False)
        for u, ref in self.R_UNORDERED.items():
            np.testing.assert_allclose(nets[u], ref, err_msg=u)

    def test_ordered(self):
        nets = self.networks(True)
        for u, ref in self.R_ORDERED.items():
            np.testing.assert_allclose(nets[u], ref, err_msg=u)


class TestSelfConnections:
    """self_connection_counts_ = R tma's self.connection.counts (test-self-connections.R)."""

    TOY = TestMatchesRAccumulate.TOY

    def accum(self, **kw):
        tensor = context_tensor(self.TOY, default_window=3.0)
        return accumulate(self.TOY, units="u", conversations="c", codes=["A", "B", "C"],
                          tensor=tensor, **kw)

    def unit_x(self, values, a):
        return values[a.unit_labels_.index("x")]

    def test_self_counts(self):
        for kw, ref in [({"weight_by": "product"}, [2, 3, 2]),
                        ({"weight_by": "binary"}, [1, 1, 1]),
                        ({"weight_by": "sqrt"}, [np.sqrt(2), np.sqrt(3), np.sqrt(2)]),
                        ({"ordered": True}, [2, 3, 2])]:
            a = self.accum(**kw)
            assert a.self_connection_counts_.shape == (len(a.unit_labels_), 3)
            np.testing.assert_allclose(self.unit_x(a.self_connection_counts_, a), ref, err_msg=str(kw))

    def test_ordered_self_equals_connection_diagonal(self):
        a = self.accum(ordered=True, weight_by="product")
        diag = [a.connection_names_.index(f"{c} & {c}") for c in ["A", "B", "C"]]
        np.testing.assert_allclose(a.self_connection_counts_, a.connection_counts_[:, diag])

    def test_self_misses(self):
        assert self.accum(misses=False).self_miss_counts_ is None
        a = self.accum(misses=True, weight_by="product")
        np.testing.assert_allclose(self.unit_x(a.self_miss_counts_, a), [3, 0, 1])


class TestSelfConnectionsInCounts:
    """accumulate(self_connections=True) = R tma's accumulate(self_connections = TRUE)
    (test-self-in-counts.R): A & A columns in the connection counts."""

    TOY = TestMatchesRAccumulate.TOY

    def accum(self, **kw):
        tensor = context_tensor(self.TOY, default_window=3.0)
        return accumulate(self.TOY, units="u", conversations="c", codes=["A", "B", "C"],
                          tensor=tensor, **kw)

    def test_columns_and_values(self):
        a = self.accum(self_connections=True, weight_by="product")
        assert a.self_connections_ is True
        assert a.connection_names_ == ["A & A", "A & B", "B & B", "A & C", "B & C", "C & C"]
        assert a.adjacency_key_[0] == ["A", "A"]
        x = a.unit_labels_.index("x")
        np.testing.assert_allclose(a.connection_counts_[x], [2, 12, 3, 9, 12, 2])
        np.testing.assert_allclose(a.connection_counts_[:, [0, 2, 5]], a.self_connection_counts_)

    def test_off_by_default_and_ignored_when_ordered(self):
        assert self.accum().connection_counts_.shape[1] == 3
        o = self.accum(ordered=True, self_connections=True)
        assert o.self_connections_ is False and o.connection_counts_.shape[1] == 9


class TestHooRules:
    """accumulate(hoo_rules=) = R tma's contexts(hoo_rules =) + accumulate():
    the getting-started vignette (chats public to the group, resources
    private to the user), against R tma's values."""

    @staticmethod
    def vignette(ordered):
        from tma import rules
        from tma.datasets import load_test_mockdata
        df = load_test_mockdata()
        df = df[df["chatGroup"] == "PAM"]
        hoo = rules(
            lambda d, u: (d["modality"] == "chat") & (d["chatGroup"] == u["chatGroup"])
                         & (d["condition"] == u["condition"]),
            lambda d, u: (d["modality"] == "resource") & (d["userID"] == u["userID"])
                         & (d["condition"] == u["condition"]),
        )
        t = context_tensor(df, mode_col="modality", default_window=0, default_weight=0)
        chat, resource = t.mode_levels.index("chat"), t.mode_levels.index("resource")
        t[chat, 1], t[resource, 1] = 360, 180
        t[chat, 0], t[resource, 0] = 1, 2
        return accumulate(df, ["userID", "condition"], codes=["A", "B", "C"],
                          hoo_rules=hoo, tensor=t, time_col="timeStamp",
                          ordered=ordered)

    def test_ordered_matches_r(self):
        a = self.vignette(ordered=True)
        assert a.unit_labels_ == ["User1::FirstHalf", "User2::FirstHalf", "User3::FirstHalf"]
        assert a.connection_names_ == ["A & A", "B & A", "C & A", "A & B", "B & B",
                                       "C & B", "A & C", "B & C", "C & C"]
        np.testing.assert_allclose(a.connection_counts_, [
            [0, 0, 0, 0, 0, 0, 2, 0, 0],
            [0, 0, 0, 2, 0, 2, 1, 2, 0],
            [1, 2, 1, 0, 0, 0, 1, 2, 0],
        ])

    def test_unordered_matches_r(self):
        a = self.vignette(ordered=False)
        assert a.connection_names_ == ["A & B", "A & C", "B & C"]
        np.testing.assert_allclose(a.connection_counts_, [[0, 2, 0], [2, 1, 2], [2, 2, 2]])

    def test_requires_conversations_or_rules(self):
        with pytest.raises(ValueError):
            accumulate(make_df(), "user", codes=CODES)
