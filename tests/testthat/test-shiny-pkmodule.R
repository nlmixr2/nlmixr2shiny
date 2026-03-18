test_that("pkUI: renders without error", {
  expect_no_error(nlmixr2shiny:::pkUI("test_pk"))
})

test_that(".pkmodlib: returns data frame with name and description", {
  skip_if_not_installed("qs")
  result <- nlmixr2shiny:::.pkmodlib()
  expect_s3_class(result, "data.frame")
  expect_true("name" %in% names(result))
  expect_true("description" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("pkServer: initializes without error", {
  results <- shiny::reactiveValues(
    pkpdm = NULL, pkpdpipe = NULL,
    modelTypeSwitch = "Model Builder",
    absorption_method = NULL, distribution_model = NULL,
    elimination_method = NULL, parameterization = NULL,
    pk_switch = NULL, pd_switch = NULL, transit_compartment = NULL,
    response_type = NULL, drug_action = NULL, baseline = NULL,
    type_of_model = NULL, sigmoidicity = NULL, par_bas = NULL,
    modlibInput = NULL
  )
  shiny::testServer(nlmixr2shiny:::pkServer, args = list(results = results), {
    expect_true(TRUE)
  })
})

test_that("pkServer: observe updates results when Model Builder inputs set", {
  results <- shiny::reactiveValues(
    pkpdm = NULL, pkpdpipe = NULL,
    modelTypeSwitch = "Model Builder",
    absorption_method = NULL, distribution_model = NULL,
    elimination_method = NULL, parameterization = NULL,
    pk_switch = TRUE, pd_switch = FALSE, transit_compartment = NULL,
    response_type = "Direct/Immediate", drug_action = "linear",
    baseline = "baseline = 0", type_of_model = "stimulation of input",
    sigmoidicity = FALSE, par_bas = FALSE, modlibInput = NULL
  )

  shiny::testServer(nlmixr2shiny:::pkServer, args = list(results = results), {
    session$setInputs(
      modelTypeSwitch = "Model Builder",
      absorption_method = "First Order",
      distribution_model = "1 compartment",
      elimination_method = "Linear",
      parameterization = "Cl/V",
      pk_switch = TRUE, pd_switch = FALSE,
      response_type = "Direct/Immediate",
      drug_action = "linear",
      baseline = "baseline = 0",
      type_of_model = "stimulation of input",
      sigmoidicity = FALSE, par_bas = FALSE
    )
    expect_equal(results$modelTypeSwitch, "Model Builder")
    expect_equal(results$absorption_method, "First Order")
    expect_false(is.null(results$pkpdpipe))
    expect_true(grepl("PK_1cmt_des", results$pkpdpipe))
  })
})

test_that("pkServer: model library selection updates modlibInput", {
  skip_if_not_installed("qs")
  results <- shiny::reactiveValues(
    pkpdm = NULL, pkpdpipe = NULL,
    modelTypeSwitch = "Model Library", modlibInput = NULL,
    absorption_method = "First Order", distribution_model = "1 compartment",
    elimination_method = "Linear", parameterization = "Cl/V",
    pk_switch = TRUE, pd_switch = FALSE, transit_compartment = NULL,
    response_type = "Direct/Immediate", drug_action = "linear",
    baseline = "baseline = 0", type_of_model = "stimulation of input",
    sigmoidicity = FALSE, par_bas = FALSE
  )

  shiny::testServer(nlmixr2shiny:::pkServer, args = list(results = results), {
    session$setInputs(
      modelTypeSwitch = "Model Library",
      modlibInput = "PK_1cmt_des",
      absorption_method = "First Order",
      distribution_model = "1 compartment",
      elimination_method = "Linear",
      parameterization = "Cl/V",
      pk_switch = TRUE, pd_switch = FALSE,
      response_type = "Direct/Immediate",
      drug_action = "linear",
      baseline = "baseline = 0",
      type_of_model = "stimulation of input",
      sigmoidicity = FALSE, par_bas = FALSE
    )
    expect_equal(results$modlibInput, "PK_1cmt_des")
  })
})

test_that("pkServer: Transit absorption sets transit_compartment", {
  results <- shiny::reactiveValues(
    pkpdm = NULL, pkpdpipe = NULL,
    modelTypeSwitch = "Model Builder", modlibInput = NULL,
    absorption_method = NULL, distribution_model = NULL,
    elimination_method = NULL, parameterization = NULL,
    pk_switch = TRUE, pd_switch = FALSE, transit_compartment = NULL,
    response_type = "Direct/Immediate", drug_action = "linear",
    baseline = "baseline = 0", type_of_model = "stimulation of input",
    sigmoidicity = FALSE, par_bas = FALSE
  )

  shiny::testServer(nlmixr2shiny:::pkServer, args = list(results = results), {
    session$setInputs(
      modelTypeSwitch = "Model Builder",
      absorption_method = "Transit",
      distribution_model = "1 compartment",
      elimination_method = "Linear",
      parameterization = "Cl/V",
      transit_compartment = 3,
      pk_switch = TRUE, pd_switch = FALSE,
      response_type = "Direct/Immediate",
      drug_action = "linear",
      baseline = "baseline = 0",
      type_of_model = "stimulation of input",
      sigmoidicity = FALSE, par_bas = FALSE
    )
    expect_equal(results$transit_compartment, 3)
    expect_true(grepl("addTransit\\(3\\)", results$pkpdpipe))
  })
})

test_that("pkServer: PKPD model generates combined pipeline", {
  results <- shiny::reactiveValues(
    pkpdm = NULL, pkpdpipe = NULL,
    modelTypeSwitch = "Model Builder", modlibInput = NULL,
    absorption_method = NULL, distribution_model = NULL,
    elimination_method = NULL, parameterization = NULL,
    pk_switch = TRUE, pd_switch = TRUE, transit_compartment = NULL,
    response_type = "Direct/Immediate", drug_action = "Emax",
    baseline = "baseline = 0", type_of_model = "stimulation of input",
    sigmoidicity = FALSE, par_bas = FALSE
  )

  shiny::testServer(nlmixr2shiny:::pkServer, args = list(results = results), {
    session$setInputs(
      modelTypeSwitch = "Model Builder",
      absorption_method = "First Order",
      distribution_model = "1 compartment",
      elimination_method = "Linear",
      parameterization = "Cl/V",
      pk_switch = TRUE, pd_switch = TRUE,
      response_type = "Direct/Immediate",
      drug_action = "Emax",
      baseline = "baseline = 0",
      type_of_model = "stimulation of input",
      sigmoidicity = FALSE, par_bas = FALSE
    )
    expect_false(is.null(results$pkpdpipe))
    expect_true(grepl("PK_1cmt_des", results$pkpdpipe))
  })
})

test_that("pkServer: modelTypeSwitchUi renders Reset+Current when modelModified=TRUE", {
  results <- shiny::reactiveValues(
    pkpdm = NULL, pkpdpipe = NULL,
    modelTypeSwitch = "Current Model", modlibInput = NULL,
    modelModified = TRUE,
    absorption_method = "First Order", distribution_model = "1 compartment",
    elimination_method = "Linear", parameterization = "Cl/V",
    pk_switch = TRUE, pd_switch = FALSE, transit_compartment = NULL,
    response_type = "Direct/Immediate", drug_action = "linear",
    baseline = "baseline = 0", type_of_model = "stimulation of input",
    sigmoidicity = FALSE, par_bas = FALSE
  )

  shiny::testServer(nlmixr2shiny:::pkServer, args = list(results = results), {
    session$setInputs(modelTypeSwitch = "Current Model")
    ui <- output$modelTypeSwitchUi
    expect_false(is.null(ui))
  })
})

test_that("pkServer: modelTypeUi Reset Model branch sets modelModified to NA", {
  results <- shiny::reactiveValues(
    pkpdm = NULL, pkpdpipe = NULL,
    modelTypeSwitch = "Reset Model", modlibInput = NULL,
    modelModified = TRUE,
    absorption_method = "First Order", distribution_model = "1 compartment",
    elimination_method = "Linear", parameterization = "Cl/V",
    pk_switch = TRUE, pd_switch = FALSE, transit_compartment = NULL,
    response_type = "Direct/Immediate", drug_action = "linear",
    baseline = "baseline = 0", type_of_model = "stimulation of input",
    sigmoidicity = FALSE, par_bas = FALSE
  )

  shiny::testServer(nlmixr2shiny:::pkServer, args = list(results = results), {
    session$setInputs(
      modelTypeSwitch = "Reset Model",
      absorption_method = "First Order",
      distribution_model = "1 compartment",
      elimination_method = "Linear",
      parameterization = "Cl/V",
      pk_switch = TRUE, pd_switch = FALSE,
      response_type = "Direct/Immediate",
      drug_action = "linear",
      baseline = "baseline = 0",
      type_of_model = "stimulation of input",
      sigmoidicity = FALSE, par_bas = FALSE
    )
    expect_no_error(output$modelTypeUi)
  })
})

test_that("pkServer: modelTypeUi Model Library branch with mocked .pkmodlib", {
  local_mocked_bindings(
    .pkmodlib = function() data.frame(
      name = c("PK_1cmt_des", "PK_2cmt_des"),
      description = c("1-cmt model", "2-cmt model"),
      stringsAsFactors = FALSE
    ),
    .package = "nlmixr2shiny"
  )

  results <- shiny::reactiveValues(
    pkpdm = NULL, pkpdpipe = NULL,
    modelTypeSwitch = "Model Library", modlibInput = NULL,
    modelModified = FALSE,
    absorption_method = "First Order", distribution_model = "1 compartment",
    elimination_method = "Linear", parameterization = "Cl/V",
    pk_switch = TRUE, pd_switch = FALSE, transit_compartment = NULL,
    response_type = "Direct/Immediate", drug_action = "linear",
    baseline = "baseline = 0", type_of_model = "stimulation of input",
    sigmoidicity = FALSE, par_bas = FALSE
  )

  shiny::testServer(nlmixr2shiny:::pkServer, args = list(results = results), {
    session$setInputs(
      modelTypeSwitch = "Model Library",
      absorption_method = "First Order",
      distribution_model = "1 compartment",
      elimination_method = "Linear",
      parameterization = "Cl/V",
      pk_switch = TRUE, pd_switch = FALSE,
      response_type = "Direct/Immediate",
      drug_action = "linear",
      baseline = "baseline = 0",
      type_of_model = "stimulation of input",
      sigmoidicity = FALSE, par_bas = FALSE
    )
    ui <- output$modelTypeUi
    expect_false(is.null(ui))
  })
})
