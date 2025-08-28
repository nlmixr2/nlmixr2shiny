
#Define the UI for the covariance module
covUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("\u03a9"),
    tags$style(HTML("
      .monospace-textarea {
        font-family: 'Courier New', Courier, monospace;
      }
    ")),
    fluidRow(
      column(
        width = 12,
        rhandsontable::rHandsontableOutput(ns("triangleTable")) # Covariance table below the button
      )
    )
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
