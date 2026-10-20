"""Tests for tma.libtma (moved from qe-lib): accumulate_unit, accumulate_unit_with_rows, apply_tensor_unit."""
import numpy as np
import pytest
# Moved from qe-lib's tests with libtma (libqe's phase 4b split). The old
# module name is aliased to tma.libtma so the test bodies are unchanged.
from tma import libtma as accumulation
from tma.libtma import UnitNetworks, TensorNetworks


# ── accumulate_unit ───────────────────────────────────────────────────────────

def window_decay(distances):
    """Simple window: weight=1 for all rows in window."""
    return np.ones(len(distances), dtype=np.float64)


class TestAccumulateUnit:
    def test_returns_1d_array(self):
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        out = accumulation.accumulate_unit(codes, [1, 2], window_decay, False)
        assert out.ndim == 1

    def test_output_length_unordered(self):
        # choose_two(3) = 3
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        out = accumulation.accumulate_unit(codes, [1, 2], window_decay, False)
        assert out.shape[0] == 3

    def test_output_length_ordered(self):
        # ordered=True → p^2 = 9
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        out = accumulation.accumulate_unit(codes, [1, 2], window_decay, True)
        assert out.shape[0] == 9

    def test_zero_codes_gives_zero_output(self):
        codes = np.zeros((4, 3), dtype=np.float64)
        out = accumulation.accumulate_unit(codes, [1, 2, 3], window_decay, False)
        assert np.all(out == 0)

    def test_first_row_only_unit_gives_zero(self):
        # Row 0 is always skipped (no ground rows before it)
        codes = np.ones((3, 2), dtype=np.float64)
        out = accumulation.accumulate_unit(codes, [0], window_decay, False)
        assert np.all(out == 0)

    def test_decay_fn_receives_correct_distances(self):
        """decay_fn for unit_row=2 should receive distances [2, 1, 0]."""
        received = []
        def recording_decay(distances):
            received.append(distances.copy())
            return np.ones(len(distances), dtype=np.float64)
        codes = np.ascontiguousarray(np.ones((4, 2)), dtype=np.float64)
        accumulation.accumulate_unit(codes, [2], recording_decay, False)
        assert len(received) == 1
        np.testing.assert_array_equal(received[0], [2.0, 1.0, 0.0])


# ── accumulate_unit_with_rows ─────────────────────────────────────────────────

def two_arg_decay(unit_row, ground_indices):
    """Simple uniform weight decay."""
    return np.ones(len(ground_indices), dtype=np.float64)


class TestAccumulateUnitWithRows:
    def test_returns_unit_networks(self):
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        out = accumulation.accumulate_unit_with_rows(codes, [1, 2], two_arg_decay, False)
        assert isinstance(out, UnitNetworks)

    def test_networks_length_unordered(self):
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        out = accumulation.accumulate_unit_with_rows(codes, [1, 2], two_arg_decay, False)
        assert out.networks.shape[0] == 3  # choose_two(3)

    def test_row_networks_shape(self):
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        out = accumulation.accumulate_unit_with_rows(codes, [1, 2], two_arg_decay, True)
        # row_networks: n_unit_rows x p^2
        assert out.row_networks.shape == (2, 9)

    def test_networks_matches_accumulate_unit(self):
        """networks field should match accumulate_unit for same inputs."""
        codes = np.ascontiguousarray(
            np.array([[1,0,1],[0,1,0],[1,1,0],[0,0,1]], dtype=np.float64))
        unit_rows = [1, 2, 3]
        r1 = accumulation.accumulate_unit(codes, unit_rows, window_decay, False)
        r2 = accumulation.accumulate_unit_with_rows(codes, unit_rows, two_arg_decay, False)
        np.testing.assert_allclose(r1, r2.networks, atol=1e-12)

    def test_decay_fn_receives_unit_row_and_indices(self):
        """Two-arg decay_fn should receive (unit_row: int, ground_indices: ndarray)."""
        calls = []
        def recording_decay(unit_row, ground_indices):
            calls.append((int(unit_row), ground_indices.copy()))
            return np.ones(len(ground_indices), dtype=np.float64)
        codes = np.ascontiguousarray(np.ones((3, 2)), dtype=np.float64)
        accumulation.accumulate_unit_with_rows(codes, [2], recording_decay, False)
        assert len(calls) == 1
        assert calls[0][0] == 2
        np.testing.assert_array_equal(calls[0][1], [0, 1, 2])


# ── apply_tensor_unit ─────────────────────────────────────────────────────────

def make_default_tensor(weight=1.0, window=3.0):
    """Default tensor: dims=[2], tensor=[weight, window]."""
    return np.array([weight, window], dtype=np.float64)

def make_context_lookup(n_rows):
    """Minimal context_lookup with 0 factor columns (default tensor path uses dims only)."""
    # For the IS_DEFAULT path (dims=[2]), context_lookup rows are only used for
    # the dims_receiver override — with empty dims_receiver this is a no-op.
    return np.zeros((n_rows, 0), dtype=np.int32)


class TestApplyTensorUnit:
    def test_returns_tensor_networks(self):
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        times = np.array([0.0, 1.0, 2.0])
        ctx   = make_context_lookup(3)
        out = accumulation.apply_tensor_unit(
            make_default_tensor(1.0, 3.0), [2],
            [], [], [],
            ctx, [1, 2], codes, times, True)
        assert isinstance(out, TensorNetworks)

    def test_connection_counts_length_ordered(self):
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        times = np.arange(3, dtype=np.float64)
        ctx   = make_context_lookup(3)
        out = accumulation.apply_tensor_unit(
            make_default_tensor(1.0, 5.0), [2],
            [], [], [],
            ctx, [1, 2], codes, times, True)
        assert out.connection_counts.shape[0] == 9  # p^2 for ordered

    def test_row_connection_counts_shape(self):
        codes = np.ascontiguousarray(np.eye(3), dtype=np.float64)
        times = np.arange(3, dtype=np.float64)
        ctx   = make_context_lookup(3)
        out = accumulation.apply_tensor_unit(
            make_default_tensor(1.0, 5.0), [2],
            [], [], [],
            ctx, [1, 2], codes, times, True)
        assert out.row_connection_counts.shape == (2, 9)

    def test_window_clips_ground_rows(self):
        """With window=1.5, only the immediately prior row should be in-window."""
        codes = np.ascontiguousarray(np.ones((4, 2)), dtype=np.float64)
        times = np.array([0.0, 1.0, 2.0, 3.0])
        ctx   = make_context_lookup(4)
        # window=1.5 → row k can only see row k-1 (gap=1 < 1.5)
        out_small = accumulation.apply_tensor_unit(
            make_default_tensor(1.0, 1.5), [2],
            [], [], [], ctx, [2], codes, times, True)
        # window=10 → row k sees all previous rows
        out_large = accumulation.apply_tensor_unit(
            make_default_tensor(1.0, 10.0), [2],
            [], [], [], ctx, [2], codes, times, True)
        # larger window means more ground rows → larger connection counts
        assert out_large.connection_counts.sum() >= out_small.connection_counts.sum()

    def test_zero_weight_gives_zero_output(self):
        codes = np.ascontiguousarray(np.ones((3, 2)), dtype=np.float64)
        times = np.arange(3, dtype=np.float64)
        ctx   = make_context_lookup(3)
        out = accumulation.apply_tensor_unit(
            make_default_tensor(0.0, 5.0), [2],
            [], [], [], ctx, [1, 2], codes, times, True)
        assert np.all(out.connection_counts == 0)


# ── aggregate_row_connections / finalize_row_connections (weight models) ──────

def weighted_row_conn():
    """Two response rows x 3 codes, directed 3x3 per row (column-major within
    the row); counts > 1 and asymmetric (same fixture as the R / wasm tests)."""
    rc = np.zeros((2, 9), dtype=np.float64)
    rc[0, 3] = 2.0    # m(0,1)
    rc[0, 1] = 1.0    # m(1,0)  -> row 0 folds to A&B = 3
    rc[0, 7] = 4.0    # m(1,2)  -> row 0 B&C = 4
    rc[1, 6] = 9.0    # m(0,2)  -> row 1 A&C = 9
    rc[1, 3] = 0.5    # m(0,1)  -> row 1 A&B = 0.5
    return rc


class TestAggregateRowConnectionsWeights:
    def agg(self, weight, ordered=False):
        return accumulation.aggregate_row_connections(weighted_row_conn(), 3, ordered, weight)

    def test_unordered_weights_apply_per_row_before_sum(self):
        np.testing.assert_allclose(self.agg("binary"),  [2, 1, 1])
        np.testing.assert_allclose(self.agg("product"), [3.5, 9, 4])
        np.testing.assert_allclose(self.agg("sqrt"),    [np.sqrt(3) + np.sqrt(0.5), 3, 2])
        np.testing.assert_allclose(self.agg("log1p"),   [np.log1p(3) + np.log1p(0.5), np.log1p(9), np.log1p(4)])
        np.testing.assert_allclose(self.agg("log"),     self.agg("log1p"))

    def test_legacy_bool_and_default(self):
        np.testing.assert_array_equal(self.agg(True),  self.agg("binary"))
        np.testing.assert_array_equal(self.agg(False), self.agg("product"))
        np.testing.assert_array_equal(
            accumulation.aggregate_row_connections(weighted_row_conn(), 3), self.agg("binary"))

    def test_ordered_binary_keeps_raw_counts(self):
        rc = weighted_row_conn()
        np.testing.assert_allclose(self.agg("binary", True),  rc.sum(axis=0))
        np.testing.assert_allclose(self.agg("product", True), rc.sum(axis=0))
        np.testing.assert_allclose(self.agg("sqrt", True),    np.sqrt(rc).sum(axis=0))

    def test_finalize_rows_sum_to_aggregate(self):
        for weight in ("binary", "product", "sqrt", "log1p"):
            for ordered in (False, True):
                fin = accumulation.finalize_row_connections(weighted_row_conn(), 3, ordered, weight)
                assert fin.shape == (2, 9 if ordered else 3)
                np.testing.assert_allclose(fin.sum(axis=0), self.agg(weight, ordered))

    def test_unknown_weight_raises(self):
        with pytest.raises(ValueError, match="Unknown weight model"):
            self.agg("cube")

    def test_tensor_rows_match_rena_weight_by(self):
        # One unit/conversation, window 3 (same example pinned in the R tests).
        codes = np.array([[2, 0, 1], [1, 3, 0], [0, 1, 2]], dtype=np.float64)
        out = accumulation.apply_tensor_unit(
            make_default_tensor(1.0, 3.0), [2], [], [], [],
            make_context_lookup(3), [0, 1, 2], codes,
            np.arange(3, dtype=np.float64), True)
        rc = np.ascontiguousarray(out.row_connection_counts)
        agg = lambda w: accumulation.aggregate_row_connections(rc, 3, False, w)
        np.testing.assert_allclose(agg("binary"),  [2, 3, 2])
        np.testing.assert_allclose(agg("product"), [12, 9, 12])
        np.testing.assert_allclose(agg("sqrt"),  [4.73205080756888, 4.86370330515627, 4.73205080756888], rtol=1e-12)
        np.testing.assert_allclose(agg("log1p"), [3.68887945411394, 3.73766961828337, 3.68887945411394], rtol=1e-12)


# ── missed co-occurrences ─────────────────────────────────────────────────────
# One unit, window 3 (tma's R test-misses.R fixture):
#   row 2: ground (2,0,1), C absent -> A->C 2, C->C 1
#   row 3: ground (3,3,1), A absent -> A->A 3, B->A 3, C->A 1
# Directed layout ground i -> response j at j*n+i.

MISS_CODES = np.array([[2, 0, 1], [1, 3, 0], [0, 1, 2]], dtype=np.float64)
ROW_MISSES = np.array([[0, 0, 0, 0, 0, 0, 0, 0, 0],
                       [0, 0, 0, 0, 0, 0, 2, 0, 1],
                       [3, 3, 1, 0, 0, 0, 0, 0, 0]], dtype=np.float64)


def _miss_tensor_unit(misses):
    return accumulation.apply_tensor_unit(
        np.array([1.0, 3.0]), [2], [], [], [],
        np.zeros((3, 0), dtype=np.int32), [0, 1, 2],
        MISS_CODES, np.array([0.0, 1.0, 2.0]), True, misses)


class TestApplyTensorUnitMisses:
    def test_row_and_unit_misses(self):
        r = _miss_tensor_unit(True)
        np.testing.assert_array_equal(r.row_miss_counts, ROW_MISSES)
        np.testing.assert_array_equal(r.miss_counts, ROW_MISSES.sum(axis=0))

    def test_off_by_default_and_hits_unchanged(self):
        plain = _miss_tensor_unit(False)
        assert plain.miss_counts is None and plain.row_miss_counts is None
        np.testing.assert_array_equal(plain.row_connection_counts,
                                      _miss_tensor_unit(True).row_connection_counts)

    def test_unordered_fold(self):
        # A & B = B->A 3; A & C = A->C 2 + C->A 1; B & C = 0
        r = _miss_tensor_unit(True)
        np.testing.assert_allclose(
            accumulation.aggregate_row_connections(r.row_miss_counts, 3, False, "product"),
            [3, 3, 0])


class TestAccumulateUnitNetworks:
    """accumulate_unit_networks: one call = fold + weight + sum + self diagonal."""

    def call(self, ordered, weight, misses=True):
        return accumulation.accumulate_unit_networks(
            np.array([1.0, 3.0]), [2], [], [], [],
            np.zeros((3, 0), dtype=np.int32), [0, 1, 2],
            MISS_CODES, np.array([0.0, 1.0, 2.0]), ordered, weight, misses)

    def test_unordered_product(self):
        u = self.call(False, "product")
        np.testing.assert_allclose(u.connection_counts, [12, 9, 12])
        assert u.row_connection_counts.shape == (3, 3)
        np.testing.assert_allclose(u.self_connection_counts, [2, 3, 2])
        np.testing.assert_allclose(u.row_self_connection_counts, [[0, 0, 0], [2, 0, 0], [0, 3, 2]])
        np.testing.assert_allclose(u.miss_counts, [3, 3, 0])
        np.testing.assert_allclose(u.self_miss_counts, [3, 0, 1])

    def test_matches_aggregating_apply_tensor_unit(self):
        rc = _miss_tensor_unit(True)
        for ordered in (False, True):
            for w in ("binary", "product", "sqrt"):
                u = self.call(ordered, w)
                np.testing.assert_allclose(u.connection_counts, accumulation.aggregate_row_connections(
                    rc.row_connection_counts, 3, ordered, w))
                np.testing.assert_allclose(u.miss_counts, accumulation.aggregate_row_connections(
                    rc.row_miss_counts, 3, ordered, w))

    def test_misses_off(self):
        u = self.call(False, "binary", misses=False)
        assert u.miss_counts is None and u.row_self_miss_counts is None

    def test_weighted_rows_sum_to_unit_vector(self):
        for ordered in (False, True):
            for w in ("binary", "product", "sqrt"):
                u = self.call(ordered, w)
                rows = accumulation.weight_rows(u.row_connection_counts, ordered, w)
                np.testing.assert_allclose(rows.sum(axis=0), u.connection_counts)


class TestDiagonalInCounts:
    """accumulate_unit_networks(diagonal=True): self-connections as columns
    A & A, A & B, B & B, A & C, B & C, C & C (unordered)."""

    def call(self, ordered, weight, diagonal):
        return accumulation.accumulate_unit_networks(
            np.array([1.0, 3.0]), [2], [], [], [],
            np.zeros((3, 0), dtype=np.int32), [0, 1, 2],
            MISS_CODES, np.array([0.0, 1.0, 2.0]), ordered, weight, True, diagonal)

    def test_layout_and_values(self):
        u = self.call(False, "product", True)
        np.testing.assert_allclose(u.connection_counts, [2, 12, 3, 9, 12, 2])
        assert u.row_connection_counts.shape == (3, 6)
        np.testing.assert_allclose(u.connection_counts[[0, 2, 5]], u.self_connection_counts)
        np.testing.assert_allclose(u.miss_counts[[0, 2, 5]], u.self_miss_counts)

    def test_off_and_ordered_unchanged(self):
        np.testing.assert_allclose(self.call(False, "product", False).connection_counts, [12, 9, 12])
        np.testing.assert_allclose(self.call(True, "product", True).connection_counts,
                                   self.call(True, "product", False).connection_counts)

    def test_aggregate_matches(self):
        rc = _miss_tensor_unit(True).row_connection_counts
        for w in ("binary", "sqrt"):
            np.testing.assert_allclose(
                accumulation.aggregate_row_connections(rc, 3, False, w, True),
                self.call(False, w, True).connection_counts)
