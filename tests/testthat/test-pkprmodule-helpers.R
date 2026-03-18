test_that("addprop: initial value", {
  result <- nlmixr2shiny:::addprop("initial value", "central")
  expect_equal(result, "addIni( central )")
})

test_that("addprop: bioavailability", {
  result <- nlmixr2shiny:::addprop("bioavailability", "depot")
  expect_equal(result, "addBioavailability( depot )")
})

test_that("addprop: rate", {
  result <- nlmixr2shiny:::addprop("rate", "central")
  expect_equal(result, "addRate( central )")
})

test_that("addprop: duration", {
  result <- nlmixr2shiny:::addprop("duration", "central")
  expect_equal(result, "addDur( central )")
})

test_that("addprop: lag time", {
  result <- nlmixr2shiny:::addprop("lag time", "depot")
  expect_equal(result, "addLag( depot )")
})

test_that("addprop: match.arg works for partial match", {
  result <- nlmixr2shiny:::addprop("initial value", "A1")
  expect_true(grepl("addIni", result))
})

test_that("modelPropToFullProp: converts ini to initial value", {
  expect_equal(nlmixr2shiny:::modelPropToFullProp("ini"), "initial value")
})

test_that("modelPropToFullProp: converts f to bioavailability", {
  expect_equal(nlmixr2shiny:::modelPropToFullProp("f"), "bioavailability")
})

test_that("modelPropToFullProp: converts alag to lag time", {
  expect_equal(nlmixr2shiny:::modelPropToFullProp("alag"), "lag time")
})

test_that("modelPropToFullProp: converts dur to duration", {
  expect_equal(nlmixr2shiny:::modelPropToFullProp("dur"), "duration")
})

test_that("modelPropToFullProp: converts rate to rate", {
  expect_equal(nlmixr2shiny:::modelPropToFullProp("rate"), "rate")
})

test_that("modelPropToFullProp: vectorized over multiple properties", {
  result <- nlmixr2shiny:::modelPropToFullProp(c("ini", "f", "alag"))
  expect_equal(result, c("initial value", "bioavailability", "lag time"))
})

test_that("modelPropIni: returns empty df when cmtProp is NULL", {
  result <- nlmixr2shiny:::modelPropIni(NULL)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_named(result, c("Compartment", "Property", "Remove"))
})

test_that("modelPropIni: populates from cmtProp", {
  cmtProp <- data.frame(
    Compartment = c("central", "depot"),
    Property = c("ini", "f"),
    stringsAsFactors = FALSE
  )
  result <- nlmixr2shiny:::modelPropIni(cmtProp)
  expect_equal(nrow(result), 2)
  expect_equal(result$Compartment, c("central", "depot"))
  expect_equal(result$Property, c("initial value", "bioavailability"))
  expect_equal(result$Remove, c("Fixed", "Fixed"))
})

test_that("pipeAllProp: returns character(0) on empty data frame", {
  df <- data.frame(
    Compartment = character(0),
    Property = character(0),
    stringsAsFactors = FALSE
  )
  result <- nlmixr2shiny:::pipeAllProp(df)
  expect_equal(result, character(0))
})

test_that("pipeAllProp: skips Fixed rows", {
  df <- data.frame(
    Compartment = c("central", "depot"),
    Property = c("Fixed", "Fixed"),
    stringsAsFactors = FALSE
  )
  result <- nlmixr2shiny:::pipeAllProp(df)
  expect_equal(result, character(0))
})

test_that("pipeAllProp: generates pipe strings for non-Fixed rows", {
  df <- data.frame(
    Compartment = c("central", "depot"),
    Property = c("initial value", "bioavailability"),
    stringsAsFactors = FALSE
  )
  result <- nlmixr2shiny:::pipeAllProp(df)
  expect_length(result, 2)
  expect_true(grepl("addIni", result[1]))
  expect_true(grepl("addBioavailability", result[2]))
})

test_that("pipeAllProp: mixes Fixed and non-Fixed rows", {
  df <- data.frame(
    Compartment = c("central", "depot"),
    Property = c("Fixed", "rate"),
    stringsAsFactors = FALSE
  )
  result <- nlmixr2shiny:::pipeAllProp(df)
  expect_length(result, 1)
  expect_true(grepl("addRate", result[1]))
})

test_that("pipeAllProp: validates data frame input", {
  expect_error(nlmixr2shiny:::pipeAllProp("not a data frame"))
})
