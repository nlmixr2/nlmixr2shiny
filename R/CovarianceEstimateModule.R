updateOmegaInModel <- function(results) {
  if (#is.null(results$parEstim) ||
    is.null(results$betweenSubjectVaribility) ||
      is.null(results$triangleTable)) {
    return()
  }
  waiter::waiter_show(html = tagList(
    waiter::spin_fading_circles(),  # A nice spinning loading indicator
    h4("updating Omega Matrix in model...")
  ))
  on.exit(waiter::waiter_hide(), add = TRUE)
  .bsv <- results$betweenSubjectVaribility

  .oldOme <- dimnames(results$parEstim$omega)[[1]]
  .rm <- setdiff(.oldOme, .bsv)
  if (length(.rm) > 0) {
    results$parEstim <- nlmixr2est::rmEta(results$parEstim, .rm)
  }
  # Now add anything that was requested
  .add <- results$parEstim$fullEtaAddExpr
  for (b in .bsv) {
    .cur <- .add[b]
    if (!is.na(.cur)) {
      eval(str2lang(.cur))
    }
  }
  #Now update the omega matrix
  .mat <- as.matrix(results$triangleTable)
  .tmat <- t(.mat)
  diag(.tmat) <- 0
  .mat <- .mat + .tmat
  results$parEstim <- ini(results$parEstim, .mat)
  results$pkpdm <- results$parEstim
  results$betweenSubjectVaribility <- NULL
  results$triangleTable <- NULL
}

#Define the UI for the covariance module
covUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("resError")),
    uiOutput(ns("omegaRows"))
  )
}

getNewResidDfForEndpoint <- function(ri, input, x, single) {
  .newRes <- ri[[x]]$resErrorModel
  .df <- rhandsontable::hot_to_r(input[[paste0("resErrorEst_", x)]])
  .transform <- ri[[x]]$transform
  .dist <- ri[[x]]$distribution

  if (.dist %in% c("Normal", "t-distribution", "Cauchy")) {
    .cols <- switch(.newRes,
                    "Additive" = c("add"),
                    "Proportional" = c("prop"),
                    "Power" = c("pow", "exp"),
                    "Additive + Proportional (Combined 1)" = c("add", "prop"),
                    "Additive + Proportional (Combined 2)" = c("add", "prop"),
                    "Additive + Proportional (Default)" = c("add", "prop"),
                    "Additive + Power (Combined 1)" = c("add", "pow", "exp"),
                    "Additive + Power (Combined 2)" = c("add", "pow", "exp"),
                    "Additive + Power (Default)" = c("add", "pow", "exp"),
                    character(0))

    if (.transform %in% c("Box-Cox",
                          "Yeo-Johnson",
                          "Logit-normal + Box-Cox",
                          "Logit-normal + Yeo-Johnson",
                          "Probit-normal + Box-Cox",
                          "Probit-normal + Yeo-Johnson")) {
      .cols <- c(.cols, "lambda")
    }
    if (.dist == "t-distribution") {
      .cols <- c(.cols, "df")
    }
    .dfNew <- lapply(.cols, function(n) {
      if (n %in% colnames(.df)) {
        return(.df[[n]])
      } else {
        .n <- ifelse(n == "exp", "c", n)
        if (single) {
          return(nlmixr2lib::defaultCombine(.n,
                                            ifelse(n %in% c("add", "prop", "pow"), "sd", "")))
        } else {
          return(nlmixr2lib::defaultCombine(x, .n,
                                            ifelse(n %in% c("add", "prop", "pow"), "sd", "")))
        }
      }
    })
    .dfNew <- as.data.frame(.dfNew)
    names(.dfNew) <- .cols
    rownames(.dfNew) <- x
    .dfNew
  } else {
    .dist2 <- .toResidName(.dist)
    .cols <- .residDistributionNames[[.dist2]]
    .dfNew <- lapply(.cols, function(n) {
      if (n %in% colnames(.df)) {
        return(.df[[n]])
      } else {
        if (single) {
          return(n)
        } else {
          return(nlmixr2lib::defaultCombine(x, n))
        }
      }
    })
    .dfNew <- as.data.frame(.dfNew)
    names(.dfNew) <- .cols
    rownames(.dfNew) <- x
    .dfNew
  }
}

# Define the Server logic for the covariance module
covServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fullMatrixDf <- reactiveVal(NULL)  # Reactive value to store the full covariance matrix

    rinfo <- reactiveVal(NULL)


    output$resError <- renderUI({
      req(results$parEstim)
      if (length(results$parEstim$predDf) == 0) {
        return(NULL)
      }
      .ri <- residInfo(results$parEstim)
      rinfo(.ri)
      results$rinfo <- .ri
      .nri <- names(.ri)
      .nri <- .nri[.nri != "_modelPars"]

      c(list(fluidRow(column(width=12, h4("Residual Errors")))),
        lapply(.nri, function(x) {
          fluidRow(
            column(
              width = 3,
              rhandsontable::rHandsontableOutput(ns(paste0("resErrorEst_", x)),
                                                 width="100%")
            ),
            column(
              width=2,
              shinyWidgets::pickerInput(
                inputId = ns(paste0("resErrorModel_", x)),
                label = "Residual Error Model",
                choices = c("",
                            "Additive",
                            "Proportional",
                            "Power",
                            "Additive + Proportional (Combined 1)",
                            "Additive + Proportional (Combined 2)",
                            "Additive + Proportional (Default)",
                            "Additive + Power (Combined 1)",
                            "Additive + Power (Combined 2)",
                            "Additive + Power (Default)"
                            ),
                selected=.ri[[x]]$resErrorModel,
                options = shinyWidgets::pickerOptions(container = "body"),
                width = "100%"
              )
            ),
            column(
              width=2,
              shinyWidgets::pickerInput(
                inputId = ns(paste0("transform_", x)),
                label = "Transformation",
                choices = c("Untransformed",
                            "Log-normal",
                            "Logit-normal",
                            "Box-Cox",
                            "Yeo-Johnson",
                            "Logit-normal + Box-Cox",
                            "Logit-normal + Yeo-Johnson",
                            "Probit-normal",
                            "Probit-normal + Box-Cox",
                            "Probit-normal + Yeo-Johnson"),
                options = shinyWidgets::pickerOptions(container = "body"),
                selected=.ri[[x]]$transform,
                width = "100%"
              )
            ),
            column(
              width=2,
              shinyWidgets::pickerInput(
                inputId = ns(paste0("distribution_", x)),
                label = "Distribution",
                choices = c("Normal", "t-distribution", "Cauchy",
                            "Poisson", "Binomial", "Beta", "Chi-Squared",
                            "Geometric", "Uniform", "Weibull", "Negative Binomial",
                            "Negative Binomial (mu)",
                            "Generalized Log-Likelihood"),
                options = shinyWidgets::pickerOptions(container = "body"),
                width = "100%"
              )
            ),
            column(
              width=3,
              h3(" ")
            ))
        }))
    })

    # Dynamically add UI output if there are between subject variability
    output$omegaRows <- renderUI({
      req(results$parEstim)
      if (is.null(results$fullOmegaShiny)) {
        results$fullOmegaShiny <-  as.data.frame(results$parEstim$fullOmegaShiny)
        fullMatrixDf(results$fullOmegaShiny)
      }
      list(titlePanel("\u03a9"),
           shinyWidgets::checkboxGroupButtons(
             inputId = ns("betweenSubjectVaribility"),
             label = "Between Subject Variability",
             choices = names(fullMatrixDf()),
             selected = dimnames(results$parEstim$omega)[[1]]
           ),
           rhandsontable::rHandsontableOutput(ns("triangleTable")) # Covariance table below the button
           )
    })

    # Render the rhandsontable (covariance matrix)
    output$triangleTable <- rhandsontable::renderRHandsontable({
      # Set row names as column names for the table
      .df <- fullMatrixDf()
      .bsv <- input$betweenSubjectVaribility
      if (!all(.bsv %in% colnames(.df))) {
        .bsv <- dimnames(results$parEstim$omega)[[1]]
        output$betweenSubjectVaribility <- .bsv
      }
      if (length(.bsv) == 0) {
        return(NULL)
      }
      .df <- .df[.bsv, .bsv, drop=FALSE]
      rhandsontable::rhandsontable(.df) |>
        rhandsontable::hot_cols(renderer = "
          function (instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            if (col > row) {
              td.style.background = 'grey';
              td.style.color = 'grey';
              cellProperties.readOnly = true;
            }
          }")
    })

    observeEvent(input$betweenSubjectVaribility, {
      results$betweenSubjectVaribility <- input$betweenSubjectVaribility
      results$triangleTable <- rhandsontable::hot_to_r(input$triangleTable)
    })

    observeEvent(input$triangleTable, {
      results$betweenSubjectVaribility <- input$betweenSubjectVaribility
      results$triangleTable <- rhandsontable::hot_to_r(input$triangleTable)
    })

    observe({
      req(results$parEstim)
      req(rinfo())
      .ri <- rinfo()
      .nri <- names(.ri)
      .nri <- .nri[.nri != "_modelPars"]

      lapply(.nri, function(x) {
        tableId <- paste0("resErrorEst_", x)
        isolate({
          # Render the table with default data
          output[[tableId]] <- rhandsontable::renderRHandsontable({
            rhandsontable::rhandsontable(.ri[[x]]$df)
          })
        })
      })
    })
    observe({
      req(rinfo())  # Ensure endpointNames() is not NULL or empty
      .ri <- rinfo()
      .nri <- names(.ri)
      .nri <- .nri[.nri != "_modelPars"]
      .single <- length(.nri) == 1
      lapply(.nri, function(x) {
        # Observe changes in the transform_ dropdown for each residual error model
        observeEvent(input[[paste0("transform_", x)]], {
          # Action to perform when the transform_ dropdown changes
          newTransform <- input[[paste0("transform_", x)]]
          if (.ri[[x]]$transform == newTransform) {
            return()
          }
          if (!(.ri[[x]]$distribution %in% c("Normal", "t-distribution", "Cauchy"))) {
            shinyWidgets::updatePickerInput(session,
                                            inputId = paste0("resErrorModel_", x),
                                            selected = .ri[[x]]$resErrorModel)
            shinyWidgets::updatePickerInput(session,
                                            inputId = paste0("transform_", x),
                                            selected = .ri[[x]]$transform)
          } else {
            .ri[[x]]$transform <- newTransform
            .dfNew <- getNewResidDfForEndpoint(.ri, input, x, .single)
            .ri[[x]]$df <- .dfNew
            rinfo(.ri)
            results$rinfo <- .ri

          }
        })
        observeEvent(input[[paste0("distribution_", x)]], {
          newDist <- input[[paste0("distribution_", x)]]
          if (.ri[[x]]$distribution == newDist) {
            return()
          }
          if (.ri[[x]]$distribution %in% c("Normal", "t-distribution", "Cauchy") &&
                !(newDist %in% c("Normal", "t-distribution", "Cauchy"))) {
            .ri[[x]]$resErrorModel <- ""
            .ri[[x]]$transform <- "Untransformed"
            shinyWidgets::updatePickerInput(session,
                                            inputId = paste0("resErrorModel_", x),
                                            selected = .ri[[x]]$resErrorModel)
            shinyWidgets::updatePickerInput(session,
                                            inputId = paste0("transform_", x),
                                            selected = .ri[[x]]$transform)
          } else if (!(.ri[[x]]$distribution %in% c("Normal", "t-distribution", "Cauchy")) &&
                       newDist %in% c("Normal", "t-distribution", "Cauchy")) {
            .ri[[x]]$resErrorModel <- "Additive"
            .ri[[x]]$transform <- "Untransformed"
            shinyWidgets::updatePickerInput(session,
                                            inputId = paste0("resErrorModel_", x),
                                            selected = .ri[[x]]$resErrorModel)
            shinyWidgets::updatePickerInput(session,
                                            inputId = paste0("transform_", x),
                                            selected = .ri[[x]]$transform)
          }

          .ri[[x]]$distribution <- newDist
          .dfNew <- getNewResidDfForEndpoint(.ri, input, x, .single)
          .ri[[x]]$df <- .dfNew
          rinfo(.ri)
          results$rinfo <- .ri
        })
        observeEvent(input[[paste0("resErrorModel_", x)]],{
          newRes <- input[[paste0("resErrorModel_", x)]]
          curDist <- input[[paste0("distribution_", x)]]
          if (!(curDist %in% c("Normal", "t-distribution", "Cauchy"))) {
            shinyWidgets::updatePickerInput(session,
                                            inputId = paste0("resErrorModel_", x),
                                            selected = .ri[[x]]$resErrorModel)
            shinyWidgets::updatePickerInput(session,
                                            inputId = paste0("transform_", x),
                                            selected = .ri[[x]]$transform)
          } else if (curDist %in% c("Normal", "t-distribution", "Cauchy") &&
                .ri[[x]]$distribution %in% c("Normal", "t-distribution", "Cauchy")) {
            # These are OK
            if (newRes == "") {
              shinyWidgets::updatePickerInput(session,
                                              inputId = paste0("resErrorModel_", x),
                                              selected = .ri[[x]]$resErrorModel)
              ## showModal(modalDialog(
              ##   title = "Invalid Residual Error Model",
              ##   paste0("For Normal, t-distribution, or Cauchy distributions, a residual error model must be selected"),
              ##   easyClose = TRUE,
              ##   footer = NULL
              ## ))
            } else {
              .ri[[x]]$resErrorModel <- newRes
              .dfNew <- getNewResidDfForEndpoint(.ri, input, x, .single)
              .ri[[x]]$df <- .dfNew
              rinfo(.ri)
              results$rinfo <- .ri
            }
          } else {
            # Non normal, t and Cauchy distributions
            return()
          }
        })
      })


    })

  })
}
