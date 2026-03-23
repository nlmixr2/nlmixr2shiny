test_that("PKph: 1-cmt First Order absorption, Linear elimination, Cl/V", {
  result <- nlmixr2shiny:::PKph("First Order", "1 compartment", "Linear", "Cl/V")
  expect_type(result, "character")
  expect_true(grepl("PK_1cmt_des", result))
  expect_false(grepl("addTransit|removeDepot|addWeibullAbs|convertMM", result))
})

test_that("PKph: IV/Infusion/Bolus absorption removes depot", {
  result <- nlmixr2shiny:::PKph("IV/Infusion/Bolus", "1 compartment", "Linear", "Cl/V")
  expect_true(grepl("removeDepot", result))
  expect_true(grepl("PK_1cmt_des", result))
})

test_that("PKph: Weibull absorption", {
  result <- nlmixr2shiny:::PKph("Weibull", "1 compartment", "Linear", "Cl/V")
  expect_true(grepl("addWeibullAbs", result))
})

test_that("PKph: Transit compartment", {
  result <- nlmixr2shiny:::PKph("Transit", "1 compartment", "Linear", "Cl/V",
                                transit_compartment = 3)
  expect_true(grepl("addTransit\\(3\\)", result))
})

test_that("PKph: Transit compartment with 1 transit", {
  result <- nlmixr2shiny:::PKph("Transit", "2 compartment", "Linear", "Cl/V",
                                transit_compartment = 1)
  expect_true(grepl("addTransit\\(1\\)", result))
  expect_true(grepl("PK_2cmt_des", result))
})

test_that("PKph: 2-cmt model", {
  result <- nlmixr2shiny:::PKph("First Order", "2 compartment", "Linear", "Cl/V")
  expect_true(grepl("PK_2cmt_des", result))
})

test_that("PKph: 3-cmt model", {
  result <- nlmixr2shiny:::PKph("First Order", "3 compartment", "Linear", "Cl/V")
  expect_true(grepl("PK_3cmt_des", result))
})

test_that("PKph: Michaelis-Menten elimination", {
  result <- nlmixr2shiny:::PKph("First Order", "1 compartment", "Michaelis-Menten", "Cl/V")
  expect_true(grepl("convertMM", result))
})

test_that("PKph: kel parameterization for 1-cmt", {
  result <- nlmixr2shiny:::PKph("First Order", "1 compartment", "Linear", "kel")
  expect_true(grepl('pkTrans\\("k"\\)', result))
})

test_that("PKph: alpha parameterization for 1-cmt", {
  result <- nlmixr2shiny:::PKph("First Order", "1 compartment", "Linear", "alpha")
  expect_true(grepl('pkTrans\\("alpha"\\)', result))
})

test_that("PKph: Cl/Vss parameterization for 2-cmt", {
  result <- nlmixr2shiny:::PKph("First Order", "2 compartment", "Linear", "Cl/Vss")
  expect_true(grepl('pkTrans\\("vss"\\)', result))
})

test_that("PKph: aob parameterization for 2-cmt", {
  result <- nlmixr2shiny:::PKph("First Order", "2 compartment", "Linear", "aob")
  expect_true(grepl('pkTrans\\("aob"\\)', result))
})

test_that("PKph: k21 parameterization for 2-cmt", {
  result <- nlmixr2shiny:::PKph("First Order", "2 compartment", "Linear", "k21")
  expect_true(grepl('pkTrans\\("k21"\\)', result))
})

test_that("PKph: k21 parameterization for 3-cmt", {
  result <- nlmixr2shiny:::PKph("First Order", "3 compartment", "Linear", "k21")
  expect_true(grepl('pkTrans\\("k21"\\)', result))
})

test_that("PKph: kel parameterization for 2-cmt", {
  result <- nlmixr2shiny:::PKph("First Order", "2 compartment", "Linear", "kel")
  expect_true(grepl('pkTrans\\("k"\\)', result))
})

test_that("PKph: alpha parameterization for 3-cmt", {
  result <- nlmixr2shiny:::PKph("First Order", "3 compartment", "Linear", "alpha")
  expect_true(grepl('pkTrans\\("alpha"\\)', result))
})

test_that("PKph: pipe separator used between steps", {
  result <- nlmixr2shiny:::PKph("IV/Infusion/Bolus", "1 compartment", "Michaelis-Menten", "Cl/V")
  expect_true(grepl("\\|>", result))
})

# ---- PDph tests ----

test_that("PDph: Direct/Immediate response, linear", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "linear")
  expect_true(grepl("addDirectLin", result))
  expect_false(grepl("addEffectCmt|addIndirect", result))
})

test_that("PDph: Effect Compartment response", {
  result <- nlmixr2shiny:::PDph("Effect Compartment", "linear")
  expect_true(grepl("addEffectCmtLin", result))
})

test_that("PDph: Indirect/Turnover stimulation of input", {
  result <- nlmixr2shiny:::PDph("Indirect/Turnover", "linear",
                                type_of_model = "stimulation of input")
  expect_true(grepl('addIndirectLin\\(stim="in"\\)', result))
})

test_that("PDph: Indirect/Turnover stimulation of output", {
  result <- nlmixr2shiny:::PDph("Indirect/Turnover", "linear",
                                type_of_model = "stimulation of output")
  expect_true(grepl('addIndirectLin\\(stim="out"\\)', result))
})

test_that("PDph: Indirect/Turnover inhibition of input", {
  result <- nlmixr2shiny:::PDph("Indirect/Turnover", "linear",
                                type_of_model = "inhibition of input")
  expect_true(grepl('addIndirectLin\\(inhib="in"\\)', result))
})

test_that("PDph: Indirect/Turnover inhibition of output", {
  result <- nlmixr2shiny:::PDph("Indirect/Turnover", "linear",
                                type_of_model = "inhibition of output")
  expect_true(grepl('addIndirectLin\\(inhib="out"\\)', result))
})

test_that("PDph: Emax without sigmoidicity", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "Emax", sigmoidicity = FALSE)
  expect_true(grepl("convertEmax\\(\\)", result))
  expect_false(grepl("Hill", result))
})

test_that("PDph: Emax with sigmoidicity (Hill constant)", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "Emax", sigmoidicity = TRUE)
  expect_true(grepl("convertEmaxHill\\(\\)", result))
})

test_that("PDph: Imax without sigmoidicity", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "Imax", sigmoidicity = FALSE)
  expect_true(grepl('convertEmax\\(emax="Imax"', result))
})

test_that("PDph: Imax with sigmoidicity", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "Imax", sigmoidicity = TRUE)
  expect_true(grepl('convertEmaxHill\\(emax="Imax"', result))
})

test_that("PDph: logarithmic drug action", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "logarithmic")
  expect_true(grepl("convertLogLin", result))
})

test_that("PDph: quadratic drug action", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "quadratic")
  expect_true(grepl("convertQuad", result))
})

test_that("PDph: linear drug action adds no converter", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "linear")
  expect_false(grepl("convert", result))
})

test_that("PDph: baseline = constant", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "linear", baseline = "constant")
  expect_true(grepl("addBaselineConst", result))
})

test_that("PDph: baseline = 1-exp", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "linear", baseline = "1-exp")
  expect_true(grepl("addBaseline1exp", result))
})

test_that("PDph: baseline = linear", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "linear", baseline = "linear")
  expect_true(grepl("addBaselineLin", result))
})

test_that("PDph: baseline = exp", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "linear", baseline = "exp")
  expect_true(grepl("addBaselineExp", result))
})

test_that("PDph: baseline = 0 adds no baseline step", {
  result_null <- nlmixr2shiny:::PDph("Direct/Immediate", "linear", baseline = NULL)
  result_zero <- nlmixr2shiny:::PDph("Direct/Immediate", "linear", baseline = "baseline = 0")
  expect_equal(result_null, result_zero)
  expect_false(grepl("addBaseline", result_zero))
})

test_that("PDph: par_bas = TRUE adds convertKinR0", {
  result <- nlmixr2shiny:::PDph("Indirect/Turnover", "linear",
                                type_of_model = "stimulation of input",
                                par_bas = TRUE)
  expect_true(grepl("convertKinR0", result))
})

test_that("PDph: par_bas = FALSE does not add convertKinR0", {
  result <- nlmixr2shiny:::PDph("Indirect/Turnover", "linear",
                                type_of_model = "stimulation of input",
                                par_bas = FALSE)
  expect_false(grepl("convertKinR0", result))
})

test_that("PDph: checkmate validation - sigmoidicity must be logical", {
  expect_error(nlmixr2shiny:::PDph("Direct/Immediate", "linear", sigmoidicity = "yes"))
})

test_that("PDph: checkmate validation - par_bas must be logical", {
  expect_error(nlmixr2shiny:::PDph("Direct/Immediate", "linear", par_bas = 1))
})

test_that("PDph: pipe separator present when multiple steps", {
  result <- nlmixr2shiny:::PDph("Direct/Immediate", "Emax", sigmoidicity = FALSE)
  expect_true(grepl("\\|>", result))
})

# ---- jPh tests ----

test_that("jPh: joins PK and PD pipelines", {
  pk <- nlmixr2shiny:::PKph("First Order", "1 compartment", "Linear", "Cl/V")
  pd <- nlmixr2shiny:::PDph("Direct/Immediate", "Emax")
  result <- nlmixr2shiny:::jPh(pk, pd)
  expect_true(grepl("PK_1cmt_des", result))
  expect_true(grepl("convertEmax", result))
  expect_true(grepl("\\|>", result))
})

test_that("jPh: handles empty PD string (pure PK)", {
  pk <- nlmixr2shiny:::PKph("IV/Infusion/Bolus", "1 compartment", "Linear", "Cl/V")
  result <- nlmixr2shiny:::jPh(pk, "")
  expect_true(grepl("PK_1cmt_des", result))
  expect_true(grepl("removeDepot", result))
  expect_false(grepl("addDirectLin|addIndirect|addEffectCmt", result))
})

test_that("jPh: validates inputs are character", {
  expect_error(nlmixr2shiny:::jPh(123, "something"))
  expect_error(nlmixr2shiny:::jPh("something", 123))
})

test_that("jPh: validates single character inputs", {
  expect_error(nlmixr2shiny:::jPh(c("a", "b"), "c"))
})
