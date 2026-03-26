skip_if_not_installed("campsismod")
library(campsismod)

# ---------------------------------------------------------------------------
# campsismodModelNames / campsismodCategories / campsismodModelsInCategory
# ---------------------------------------------------------------------------

test_that("campsismodModelNames returns a non-empty named character vector", {
  nms <- campsismodModelNames()
  expect_type(nms, "character")
  expect_gt(length(nms), 0L)
  expect_equal(names(nms), unname(nms))
  # All entries are of the form "category/model"
  expect_true(all(grepl("/", nms, fixed = TRUE)))
})

test_that("campsismodCategories returns known top-level categories", {
  cats <- campsismodCategories()
  expect_true("pk" %in% cats)
  expect_true("pd" %in% cats)
})

test_that("campsismodModelsInCategory returns pk models", {
  mods <- campsismodModelsInCategory("pk")
  expect_gt(length(mods), 0L)
  expect_true("1cpt_fo" %in% mods)
})

test_that("campsismodModelsInCategory returns character(0) for unknown category", {
  expect_equal(campsismodModelsInCategory("does_not_exist"), character(0))
})

# ---------------------------------------------------------------------------
# campsismodGetModel
# ---------------------------------------------------------------------------

test_that("campsismodGetModel retrieves a campsis_model object", {
  m <- campsismodGetModel("pk/1cpt_fo")
  expect_s4_class(m, "campsis_model")
})

# ---------------------------------------------------------------------------
# campsismodToRxUi: basic structure
# ---------------------------------------------------------------------------

test_that("campsismodToRxUi returns an rxUi object for pk/1cpt_fo", {
  m <- campsismodGetModel("pk/1cpt_fo")
  rxui <- campsismodToRxUi(m)
  expect_true(inherits(rxui, "rxUi") || is.environment(rxui))
})

test_that("campsismodToRxUi iniDf contains theta rows with correct names", {
  m <- campsismodGetModel("pk/1cpt_fo")
  rxui <- campsismodToRxUi(m)
  idf <- rxui$iniDf
  expect_s3_class(idf, "data.frame")
  theta_names <- idf$name[!is.na(idf$ntheta)]
  expect_true(all(c("THETA_BIO", "THETA_KA", "THETA_VC", "THETA_CL") %in% theta_names))
})

test_that("campsismodToRxUi iniDf contains eta rows for omega parameters", {
  m <- campsismodGetModel("pk/1cpt_fo")
  rxui <- campsismodToRxUi(m)
  idf <- rxui$iniDf
  eta_names <- idf$name[!is.na(idf$neta1)]
  expect_true(all(c("ETA_KA", "ETA_VC", "ETA_CL") %in% eta_names))
})

test_that("campsismodToRxUi iniDf does NOT contain sigma/EPS parameter rows", {
  m <- campsismodGetModel("pk/1cpt_fo")
  rxui <- campsismodToRxUi(m)
  idf <- rxui$iniDf
  # Sigma matrix for this model has EPS_PROP_RUV; it must not appear in iniDf
  sigma_mat <- campsismod::rxodeMatrix(m, type = "sigma")
  sigma_param_names <- paste0("EPS_", rownames(sigma_mat))
  expect_false(any(sigma_param_names %in% idf$name))
})

test_that("campsismodToRxUi attaches sigma matrix as rxui$meta$sigma", {
  m <- campsismodGetModel("pk/1cpt_fo")
  rxui <- campsismodToRxUi(m)
  sig <- rxui$meta$sigma
  expect_true(!is.null(sig))
  expect_true(is.matrix(sig))
  expected_sigma <- campsismod::rxodeMatrix(m, type = "sigma")
  expect_equal(sig, expected_sigma)
})

test_that("sigma matrix values match campsismod::rxodeMatrix output", {
  m <- campsismodGetModel("pk/1cpt_fo")
  rxui <- campsismodToRxUi(m)
  sig <- rxui$meta$sigma
  # EPS_PROP_RUV has value 0.1 SD => variance 0.01 in the matrix
  expect_equal(sig["EPS_PROP_RUV", "EPS_PROP_RUV"], 0.01, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# campsismodToRxUi: theta initial estimates
# ---------------------------------------------------------------------------

test_that("campsismodToRxUi theta estimates match rxodeParams values", {
  m <- campsismodGetModel("pk/1cpt_fo")
  rxui <- campsismodToRxUi(m)
  idf <- rxui$iniDf
  params <- campsismod::rxodeParams(m)
  for (nm in names(params)) {
    row <- idf[idf$name == nm, , drop = FALSE]
    expect_equal(nrow(row), 1L, info = paste("theta", nm, "should have one row in iniDf"))
    expect_equal(row$est, params[[nm]], tolerance = 1e-6,
                 info = paste("theta estimate for", nm))
  }
})

# ---------------------------------------------------------------------------
# campsismodToRxUi: omega initial estimates
# ---------------------------------------------------------------------------

test_that("campsismodToRxUi omega diagonal estimates match rxodeMatrix values", {
  m <- campsismodGetModel("pk/1cpt_fo")
  rxui <- campsismodToRxUi(m)
  idf <- rxui$iniDf
  omega <- campsismod::rxodeMatrix(m, type = "omega")
  for (nm in rownames(omega)) {
    row <- idf[idf$name == nm, , drop = FALSE]
    expect_equal(nrow(row), 1L, info = paste("eta", nm, "should appear in iniDf"))
    expect_equal(row$est, omega[nm, nm], tolerance = 1e-6,
                 info = paste("omega diagonal for", nm))
  }
})

# ---------------------------------------------------------------------------
# campsismodToRxUi: model with no etas (pd model without IIV)
# ---------------------------------------------------------------------------

test_that("campsismodToRxUi handles pd models with etas", {
  m <- campsismodGetModel("pd/direct_effect_model")
  rxui <- campsismodToRxUi(m)
  idf <- rxui$iniDf
  expect_s3_class(idf, "data.frame")
  expect_gt(nrow(idf), 0L)
})
