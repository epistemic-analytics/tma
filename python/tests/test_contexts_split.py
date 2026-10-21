"""accumulate() matches R tma's on data with interleaved conversations and
units in several conversations: unsplit (time = each row's id within its
context, = R's CONTEXT_COL_ID) and split by conversation (split_by = R's
split_rules). Reference: fixtures/contexts-split.R."""
import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest

from tma import accumulate, context_tensor, contexts

FIXTURE = json.loads((Path(__file__).parent / "fixtures/contexts-split.json").read_text())
TOY = pd.DataFrame(FIXTURE["toy"]).astype({c: float for c in FIXTURE["codes"]})
VARIANTS = {
    "binary":  dict(),
    "product": dict(weight_by="product"),
    "ordered": dict(ordered=True, weight_by="product"),
}


@pytest.mark.parametrize("name", list(FIXTURE["cases"]))
def test_matches_r_tma(name):
    split, window, variant = name.split("_")
    a = accumulate(TOY, "u", "c", FIXTURE["codes"],
                   tensor=context_tensor(TOY, default_window=float(window)),
                   split_by="c" if split == "split" else None, misses=False,
                   **VARIANTS[variant])
    ref = FIXTURE["cases"][name]
    order = [a.unit_labels_.index(u) for u in ref["units"]]
    np.testing.assert_allclose(a.connection_counts_[order], ref["counts"], err_msg=name)


def test_split_contexts_follow_first_appearance():
    cm = contexts(TOY, "u", "c", split_by="c")
    for unit, splits in cm.splits_map.items():
        assert sorted(r for s in splits for r in s) == cm.contexts_map[unit]
        convs = [TOY.loc[s[0], "c"] for s in splits]
        assert len(set(convs)) == len(convs)
        assert all(TOY.loc[s, "c"].nunique() == 1 for s in splits)
        assert [s[0] for s in splits] == sorted(s[0] for s in splits)


def test_unsplit_is_one_context_and_bad_column_raises():
    cm = contexts(TOY, "u", "c")
    assert all(cm.splits_map[u] == [cm.contexts_map[u]] for u in cm.unit_labels)
    with pytest.raises(KeyError, match="split_by"):
        contexts(TOY, "u", "c", split_by="nope")


def test_split_rows_sum_to_unit_counts():
    a = accumulate(TOY, "u", "c", FIXTURE["codes"], tensor=context_tensor(TOY, default_window=3.0),
                   split_by="c", weight_by="product")
    units = TOY["u"].tolist()
    for i, u in enumerate(a.unit_labels_):
        rows = [r for r, v in enumerate(units) if v == u]
        np.testing.assert_allclose(a.row_connection_counts_[rows].sum(axis=0), a.connection_counts_[i])
