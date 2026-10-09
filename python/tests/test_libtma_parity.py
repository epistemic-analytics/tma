"""tma.libtma and qe-lib's qe.accumulation (kept until libqe 0.2.0) must give
identical results: the code moved verbatim.  Skipped once qe-lib drops it."""
import numpy as np
import pytest

from tma import libtma

# qe.accumulation is an attribute of qe-lib's extension, not an importable path.
qe_acc = getattr(pytest.importorskip("qe"), "accumulation", None)
if qe_acc is None:
    pytest.skip("qe-lib no longer has accumulation", allow_module_level=True)

rng = np.random.default_rng(7)
CODES = (rng.random((8, 3)) < 0.5).astype(np.float64)
COUNTS = rng.integers(0, 3, (8, 3)).astype(np.float64)
ROW_CONN = rng.integers(0, 3, (4, 9)).astype(np.float64)


def decay(distances):
    return np.where(distances < 3, 1.0, 0.0)


def decay2(unit_row, ground_indices):
    return np.ones(len(ground_indices), dtype=np.float64)


CASES = {
    "connection_matrix":   lambda m: m.connection_matrix(CODES[0], CODES[1], 0.5, True),
    "accumulate_stanza":   lambda m: m.accumulate_stanza(COUNTS, 3, 1, False, False),
    "accumulate_stanza_o": lambda m: m.accumulate_stanza(COUNTS, 3, 0, True, True),
    "row_connections":     lambda m: m.row_connections(COUNTS, False),
    "rolling_window_sum":  lambda m: m.rolling_window_sum(COUNTS, 3),
    "flat_index":          lambda m: m.flat_index([1, 2, 3], [2, 3, 4]),
    "accumulate_unit":     lambda m: m.accumulate_unit(COUNTS, [2, 5, 7], decay, False),
    "accumulate_unit_with_rows":
                           lambda m: (lambda r: (r.networks, r.row_networks))(
                               m.accumulate_unit_with_rows(COUNTS, [2, 5], decay2, True)),
    "apply_tensor_unit":   lambda m: (lambda r: (r.connection_counts, r.row_connection_counts))(
                               m.apply_tensor_unit(np.array([1.0, 3.0]), [2], [], [], [],
                                                   np.zeros((8, 0), dtype=np.int32),
                                                   [3, 4, 5], COUNTS, np.arange(8.0), True)),
    "aggregate_row_connections":
                           lambda m: m.aggregate_row_connections(ROW_CONN, 3, False, "sqrt"),
    "finalize_row_connections":
                           lambda m: m.finalize_row_connections(ROW_CONN, 3, True, "log1p"),
}


@pytest.mark.parametrize("name", sorted(CASES))
def test_matches_qe_lib(name):
    ours, theirs = CASES[name](libtma), CASES[name](qe_acc)
    if not isinstance(ours, tuple):
        ours, theirs = (ours,), (theirs,)
    for a, b in zip(ours, theirs):
        np.testing.assert_array_equal(np.asarray(a), np.asarray(b))
