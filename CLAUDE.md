# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```r
# Install dependencies
devtools::install_deps(dependencies = TRUE)

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-fullOmegaShiny.R")

# Check the package
devtools::check()

# Document (roxygen2)
devtools::document()

# Test coverage
covr::package_coverage()
```

## Architecture

**nlmixr2shiny** is a Shiny-based UI for building and refining PK/PD models interactively. It wraps nlmixr2lib (model library), nlmixr2est (estimation), and rxode2 (ODE solver/model representation).

### Central State

All modules share a reactive `results` list containing:
- `pkpdm` / `parEstim` — rxUi model objects
- `iniDf` — parameter initial values data frame
- `modelTypeSwitch` — current model source ("Model Builder" / "Model Library" / "Current Model")
- `ace` — code editor content
- `modelModified` — dirty flag

### Module Flow

```
pkmodule.R / pkprmodule.R   (model selection UI)
        ↓
generalModule.R              (model initialization, parses editor/library input)
        ↓
parameterEstimateModule.R    (initial estimates table, back-transformation: exp/expit/probit)
CovarianceEstimateModule.R   (omega/covariance matrix UI)
        ↓
updateOmegaInModel.R         (writes omega changes back to rxUi)
updateResidInModel.R         (writes residual error changes back to rxUi)
```

### Key Helper Files

| File | Role |
|------|------|
| `rxUiOmegaInModel.R` | Extracts random effects (eta) from an rxUi object |
| `rxUiResidDf.R` | Extracts residual error spec as a data frame |
| `rxUiTranEst.R` | Transforms/back-transforms parameter estimates |
| `getNewResidDfForEndpoint.R` | Updates residual specs per endpoint |
| `aceEditor.R` | Ace editor Shiny integration for model code |
| `zzz.R` | Registers S3 methods on load (`rxUiGet.fullOmegaShiny`, `rxUiGet.fullEtaAddExpr`) |

### S3 Extension Pattern

The package extends rxode2 via `.onLoad` in `zzz.R` by registering methods into `rxode2::rxUiGet.*` — this is how custom omega/eta extractors are injected without modifying rxode2 itself.
