
#Define the UI for the covariance module
covUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 3,
        ""
      ),
      column(
        width=2,
        shinyWidgets::pickerInput(
          inputId = "resErrorModel",
          label = "Residual Error Model",
          choices = c("Additive", "Proportional", "Combined1", "Combined2"),
          options = shinyWidgets::pickerOptions(container = "body"),
          width = "100%"
        )
      ),
      column(
        width=2,
        shinyWidgets::pickerInput(
          inputId = "transform",
          label = "Transformation",
          choices = c("Untransformed", "Lognormal", "Logit-Normal",
                      "Probit-Normal", "boxCox", "yeoJohnson"),
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

    # Reactive value to hold the updated matrix data from UI
    updatedMatrixDF <- reactiveVal(NULL)

    # Create a reactive expression to hold the initial matrix data
    matrixDF <- reactive({
      req(results$forCov)  # Ensure results$forCov is available
      matrixData <- results$forCov$omega  # Access the omega matrix
      as.data.frame(matrixData)  # Convert to data frame
    })

    # Dynamically add UI output if there are between subject variability
    output$omegaRows <- renderUI({
      if (length(results$forCov$eta) == 0) {
        NULL
      } else {
        list(
          titlePanel("\u03a9"),
          fluidRow(
            column(
              width = 12,
              rhandsontable::rHandsontableOutput(ns("triangleTable")) # Covariance table below the button
            )
          )
        )
      }
    })


    # Render the rhandsontable (covariance matrix)
    output$triangleTable <- rhandsontable::renderRHandsontable({
      df <- matrixDF()  # Access the reactive data frame

      # Set row names as column names for the table
      colnames(df) <- rownames(df)

      rhandsontable::rhandsontable(df) |>
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

    # Observe changes made to the rhandsontable and update the reactive value
    observe({
      if (!is.null(input$triangleTable)) {
        updatedMatrix <- rhandsontable::hot_to_r(input$triangleTable)  # Capture changes from UI
        updatedMatrixDF(updatedMatrix)  # Update the reactiveVal with the modified matrix
      }
    })

    # Store the expression in reactiveVals `results` to be used in other modules
    observe({
      req(updatedMatrixDF())  # Ensure the matrix has been updated
    })
  })
}
