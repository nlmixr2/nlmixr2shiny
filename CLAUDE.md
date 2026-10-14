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

## CRAN tests: never assert on timing

- Never put a wall-clock timing expectation in a test that can run on CRAN:
  no `expect_lt()`/`expect_true()` on `Sys.time()` differences,
  `system.time()`, `proc.time()`, `bench`/`microbenchmark` results, or any
  other elapsed-time threshold. CRAN check machines are shared and heavily
  loaded, so elapsed time there is random and a timing assertion fails at
  random. Example: nlmixr2plot `test-model-diagram.R` asserted a
  `modelGraph()` call took < 30 s; on CRAN it took 48.4 s and failed.
- Test the result, not the speed. If a speed regression guard is really
  needed, put it only in a test that starts with `skip_on_cran()` (or the
  package's own CRAN skip helper), and never in examples or vignettes.
- Tests that take more than a few seconds should also skip on CRAN.
