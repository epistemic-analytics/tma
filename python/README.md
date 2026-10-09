# qe-tma

Python Transmodal Analysis (TMA) — sister package to the
[tma](https://gitlab.com/epistemic-analytics/qe-packages/tma) R package
(`import tma`). Formerly pyTMA (`import pytma`), which was never published.

The math runs in C++ shared with R: accumulation (stanza windows, context
tensors, row connections, weight models) is libtma, compiled into this package
(`tma.libtma`); node positions come from [qe-ena](https://qe-libs.org/py/project/qe-ena/)
(libena) and generic numerics from [qe-lib](https://qe-libs.org/py/project/qe-lib/)
(libqe).

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
pip install qe-lib qe-ena --extra-index-url https://qe-libs.org/py/simple/
pip install "./python[dev]"
pytest python/tests --import-mode=importlib   # tests the installed package
```

## Quick start

```python
import pandas as pd
from tma import TMA, contexts, context_tensor

# rules / contexts / context_tensor mirror tma's R functions; TMA().fit()
# runs contexts -> accumulate -> normalize -> SVD -> node positions.
```

The low-level accumulation functions (`apply_tensor_unit`,
`aggregate_row_connections`, `accumulate_stanza`, ...) are in `tma.libtma`.
