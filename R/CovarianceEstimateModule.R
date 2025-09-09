
updateOmegaInModel <- function(results) {
  if (is.null(results$parEstim) ||
        is.null(results$betweenSubjectVaribility) ||
        is.null(results$triangleTable)) {
    return()
  }
}

#Define the UI for the covariance module
covUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 3,
        rhandsontable::rHandsontableOutput(ns("resErrorEst"),
                                           width="100%")
      ),
      column(
        width=2,
        shinyWidgets::pickerInput(
          inputId = "resErrorModel",
          label = "Residual Error Model",
          choices = c("Additive",
                      "Proportional",
                      "Power",
                      "Additive + Proportional (Combined 1)",
                      "Additive + Proportional (Combined 2)",
                      "Additive + Power (Combined 1)",
                      "Additive + Power (Combined 2)"
                      ),
          options = shinyWidgets::pickerOptions(container = "body"),
          width = "100%"
        )
      ),
      column(
        width=2,
        shinyWidgets::pickerInput(
          inputId = "transform",
          label = "Transformation",
          choices = c("Untransformed",
                      "Log-normal",
                      "Logit-normal",
                      "Logit-normal + Box-Cox",
                      "Logit-normal + Yeo-Johsnon",
                      "Probit-normal",
                      "Probit-normal + Box-Cox",
                      "Probit-normal + Yeo-Johsnon"),
          options = shinyWidgets::pickerOptions(container = "body"),
          width = "100%"
        )
      ),
      column(
        width=2,
        shinyWidgets::pickerInput(
          inputId = "Distribution",
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
        width = 2,
        ""
      )
    ),
    uiOutput(ns("omegaRows"))
  )
}

# Define the Server logic for the covariance module
covServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fullMatrixDf <- reactiveVal(NULL)  # Reactive value to store the full covariance matrix

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

    output$resErrorEst <- rhandsontable::renderRHandsontable({
      data.frame("additive sd"="add.sd",
                 "proportional sd"="prop.sd",
                 check.names = FALSE,
                 row.names=c("var")) |>
        rhandsontable::rhandsontable()
    })

    observeEvent(input$triangleTable, {
      results$betweenSubjectVaribility <- results$initalEstimates
      results$triangleTable <- rhandsontable::hot_to_r(input$triangleTable)
    })
  })
}
