"""tma.datasets — sample data shipped with qe-tma (= R tma's ``data()``)."""
from __future__ import annotations

from importlib import resources

import pandas as pd


def load_test_mockdata() -> pd.DataFrame:
    """Small multimodal mock data (= R ``data("test_mockdata", package = "tma")``).

    24 events by three users in two chat groups (``PAM``, ``Electric``):
    ``userID``, ``role``, ``modality`` (``"chat"`` or ``"resource"``),
    ``timeStamp`` (epoch seconds), ``chatGroup``, ``condition``,
    ``language``, ``handwriting`` and binary codes ``A``, ``B``, ``C``.
    """
    with resources.files("tma").joinpath("data/test_mockdata.csv").open() as f:
        return pd.read_csv(f)
