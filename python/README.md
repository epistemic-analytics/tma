# qe-tma

Python Transmodal Analysis (TMA) — sister package to the
[tma](https://gitlab.com/epistemic-analytics/qe-packages/tma) R package
(`import tma`). Formerly pyTMA (`import pytma`), which was never published.

qe-tma accumulates, as R's tma does: accumulation (stanza windows, context
tensors, row connections, weight models) is libtma, compiled into this package
(`tma.libtma`), with generic numerics from
[qe-lib](https://qe-libs.org/py/project/qe-lib/) (libqe). Modeling the
accumulated networks (normalization, rotation, node positions) is
[qe-ena](https://qe-libs.org/py/project/qe-ena/)'s, as rENA models tma's
accumulation in R.

## Installation

```bash
pip install qe-tma --extra-index-url https://qe-libs.org/py/simple/
```

Development builds from `main` are on `https://qe-libs.org/py/dev/simple/`.

### Development install

Building from a checkout compiles the libtma extension: it needs a C++17
compiler, CMake and [Armadillo](https://arma.sourceforge.net/) (`brew install
armadillo`, `apt install libarmadillo-dev`), plus the headers that
`scripts/sync-headers.sh` vendors into `python/include/` (libtma from this repo,
libqe from Conan; set `LIBQE_INCLUDE=<libqe checkout>/include` to use a local
libqe instead).

```bash
sh scripts/sync-headers.sh                 # from the repo root
pip install qe-lib --extra-index-url https://qe-libs.org/py/simple/
pip install "./python[dev]"
pytest python/tests --import-mode=importlib   # tests the installed package
```

## Quick start

```python
from tma import accumulate, context_tensor
from tma.datasets import load_test_mockdata

data = load_test_mockdata()     # = R's data("test_mockdata", package = "tma")

# rules / contexts / context_tensor / accumulate mirror tma's R functions.
accum = accumulate(data, units="userID", conversations="chatGroup",
                   codes=["A", "B", "C"],
                   tensor=context_tensor(data, default_window=4), ordered=True,
                   split_by="chatGroup")   # windows stay within a conversation, as rENA
accum.connection_counts_        # units x connections (= R connection.counts)

# Model it with qe-ena (pip install qe-ena; 0.1.10 or later):
from ena import ENA
model = ENA().fit(accum)
model.points_
```

For a full walkthrough (horizon-of-observation rules, per-modality time
windows and weights, directed vs. undirected networks), see
[Getting Started with TMA in Python](https://gitlab.com/epistemic-analytics/qe-packages/tma/-/blob/main/python/docs/getting-started.ipynb),
the Python port of the R package's getting-started vignette.

The low-level accumulation functions (`apply_tensor_unit`,
`aggregate_row_connections`, `accumulate_stanza`, ...) are in `tma.libtma`.
