# TMA — Transmodal Analysis

TMA is an R package for **Transmodal Analysis**, a computational framework for
analyzing complex multimodal data in Quantitative Ethnography. It extends
state-dependent models such as Epistemic Network Analysis (ENA) to account for
diverse data streams — addressing challenges like varying temporal scales and
learner characteristics — to improve the robustness and interpretability of
findings.

TMA imports, preprocesses, and accumulates coded data across modalities with
granular control over windows and weights, producing connection structures that
downstream packages such as [rENA](https://qe-libs.org/cran/pkg/rENA/) and
[ONA](https://qe-libs.org/cran/pkg/ona/) can model and visualize.

For methodological details, see Shaffer, Wang, and Ruis (2025), "Transmodal
Analysis," <https://doi.org/10.18608/jla.2025.8423>.

## Documentation

Full function reference, changelog, and articles are published at:

- **<https://qe-libs.org/cran/pkg/tma/>**

## Installation

TMA is distributed through the Epistemic Analytics CRAN-style repository at
[qe-libs.org/cran](https://qe-libs.org/cran/). Add the repository, then install
as you would any CRAN package. TMA depends on `libqe`, which is served from the
same repository, so keep the `cranqe` entry in `repos`.

```r
# Register the repositories once per session (cranqe first, then CRAN):
options(repos = c(
  cranqe = "https://qe-libs.org/cran",
  CRAN   = "https://cloud.r-project.org"
))

install.packages("tma")
```

Or as a single call:

```r
install.packages(
  "tma",
  repos = c(
    cranqe = "https://qe-libs.org/cran",
    CRAN   = "https://cloud.r-project.org"
  )
)
```

> `cran.qe-libs.org` also works and resolves to the same repository.

### Development builds

Every push to `main` publishes a development build with a `.9000`-style version
suffix. Install a specific development version (see the version listed in the
[repository index](https://qe-libs.org/cran/)) with:

```r
remotes::install_version(
  "tma",
  version = "0.3.2.9000",  # replace with the version shown in the index
  repos = c(
    cranqe = "https://qe-libs.org/cran",
    CRAN   = "https://cloud.r-project.org"
  )
)
```

## Source

Source code and issues live on GitLab:
<https://gitlab.com/epistemic-analytics/qe-packages/tma>

## License

GPL-3. See [LICENSE](LICENSE).
