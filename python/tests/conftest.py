"""Shared fixtures (moved from qe-lib's conftest.py with the accumulation tests)."""
import numpy as np
import pytest


@pytest.fixture
def codes_binary():
    """Small 4×3 binary code matrix used across accumulation tests."""
    return np.array([
        [1, 1, 0],
        [1, 0, 1],
        [0, 1, 1],
        [1, 0, 0],
    ], dtype=np.float64)


@pytest.fixture
def codes_continuous():
    """4×3 continuous (non-binary) code matrix."""
    return np.array([
        [2.0, 3.0, 0.0],
        [1.0, 0.0, 2.0],
        [0.0, 1.5, 1.0],
        [3.0, 0.0, 0.5],
    ], dtype=np.float64)
