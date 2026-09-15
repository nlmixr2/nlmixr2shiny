# Define the UI for the covariance module
covUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    uiOutput(ns("resError")),
    uiOutput(ns("omegaRows"))
  )
}

# Distributions offered by the "Distribution" picker.  These labels are the
# ones `.residDistribution()` reports and `.toResidName()` understands.
.residDistributionChoices <- c("Normal", "t-distribution", "Cauchy",
                               "Poisson", "Binomial", "Beta", "Chi-Squared",
                               "Geometric", "Uniform", "Weibull",
                               "Negative Binomial", "Negative Binomial (mu)",
                               "Generalized Log-Likelihood")

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
      .nri <- .residEndpoints(.ri)

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
                choices = .residDistributionChoices,
                # Without this the picker would come up as "Normal" no matter
                # what the endpoint actually uses, and the observer below would
                # then treat that as a user-requested change
                selected = intersect(.ri[[x]]$distribution,
                                     .residDistributionChoices),
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
        shinyWidgets::updateCheckboxGroupButtons(
          session,
          inputId = "betweenSubjectVaribility",
          # character(0) clears the buttons; NULL would leave them alone
          selected = if (is.null(.bsv)) character(0) else .bsv)
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

    # Wire the residual error table and the residual pickers of an endpoint up
    # exactly once.
    #
    # Both used to be (re)created inside an `observe()` that depended on the
    # whole of `rinfo()`.  Every picker change therefore built a fresh
    # generation of observers, each closing over the `rinfo()` value that was
    # current when it was built, and each firing immediately on creation
    # (`observeEvent()` does not ignore its init).  With more than one endpoint
    # -- any PK/PD model -- the generations then wrote their stale copies over
    # each other in turn and the tab never settled: the residual and omega
    # tables stayed stuck "recalculating", so the residual parameter names were
    # blank and pressing an eta button did nothing.
    #
    # Now the renderer depends on `rinfo()` reactively instead of capturing it,
    # and the handlers read the current value when they run, so nothing has to
    # be rebuilt when the residual specification changes.
    .wired <- new.env(parent=emptyenv())

    observe({
      req(results$parEstim)
      req(rinfo())
      lapply(.residEndpoints(rinfo()), function(x) {
        if (exists(x, envir=.wired, inherits=FALSE)) {
          return(NULL)
        }
        assign(x, TRUE, envir=.wired)
        output[[paste0("resErrorEst_", x)]] <-
          rhandsontable::renderRHandsontable({
            .ri <- rinfo()
            req(.ri[[x]])
            rhandsontable::rhandsontable(.ri[[x]]$df)
          })
        # Observe changes in the transform_ dropdown for each residual error model
        observeEvent(input[[paste0("transform_", x)]], {
          # Action to perform when the transform_ dropdown changes
          .ri <- rinfo()
          req(.ri[[x]])
          .single <- length(.residEndpoints(.ri)) == 1
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
          .ri <- rinfo()
          req(.ri[[x]])
          .single <- length(.residEndpoints(.ri)) == 1
          newDist <- input[[paste0("distribution_", x)]]
          if (.ri[[x]]$distribution == newDist) {
            return()
          }
          if (!(.ri[[x]]$distribution %in% .residDistributionChoices)) {
            # The model uses a distribution this picker cannot represent, so
            # the picker fell back to its first choice; leave the model alone
            # rather than silently rewriting it.
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
          .ri <- rinfo()
          req(.ri[[x]])
          .single <- length(.residEndpoints(.ri)) == 1
          newRes <- input[[paste0("resErrorModel_", x)]]
          curDist <- input[[paste0("distribution_", x)]]
          if (.ri[[x]]$resErrorModel == newRes) {
            return()
          }
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
        NULL
      })
    })
  })
}
