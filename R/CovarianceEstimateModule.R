
covUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("\u03a9"),
    rHandsontableOutput(ns("triangleTable")) # Use namespace for module output
  )
}



# covServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     observe({
#       req(results$forCov)  # Make sure results$forCov is available
#       matrixData <- results$forCov$omega  # This is now the accessed value
#       matrixDF <- as.data.frame(matrixData)
#     })
#     
#     # Create a sample matrix and convert to data frame
#     
# 
#     # Render the rhandsontable
#     output$triangleTable <- renderRHandsontable({
#       colnames(matrixDF) <- rownames(matrixDF)  # Use row names as column names
# 
#       rhandsontable(matrixDF) %>%
#         hot_cols(renderer = "
#           function (instance, td, row, col, prop, value, cellProperties) {
#             Handsontable.renderers.TextRenderer.apply(this, arguments);
#             if (col > row) {
#              td.style.background = 'grey';
#              td.style.color = 'grey';
#              cellProperties.readOnly = true;
#             }
#           }")
#     })
#   })
# }

# covServer <- function(id, results) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     
#     # Reactive value to hold the updated matrix data from UI
#     updatedMatrixDF <- reactiveVal(NULL)
#     
#     # Create a reactive expression to hold the initial matrix data
#     matrixDF <- reactive({
#       req(results$forCov)  # Ensure results$forCov is available
#       matrixData <- results$forCov$omega  # Access the omega matrix
#       as.data.frame(matrixData)  # Convert to data frame
#     })
#     
#     # Render the rhandsontable
#     output$triangleTable <- renderRHandsontable({
#       df <- matrixDF()  # Access the reactive data frame
#       
#       # Set row names as column names for the table
#       colnames(df) <- rownames(df)
#       
#       rhandsontable(df) %>%
#         hot_cols(renderer = "
#           function (instance, td, row, col, prop, value, cellProperties) {
#             Handsontable.renderers.TextRenderer.apply(this, arguments);
#             if (col > row) {
#               td.style.background = 'grey';
#               td.style.color = 'grey';
#               cellProperties.readOnly = true;
#             }
#           }")
#     })
#     
#     # Observe changes made to the rhandsontable and update the reactive value
#     observe({
#       if (!is.null(input$triangleTable)) {
#         updatedMatrix <- hot_to_r(input$triangleTable)  # Capture changes from UI
#         updatedMatrixDF(updatedMatrix)  # Update the reactiveVal with the modified matrix
#       }
#     })
#     
#     # Store the expression in reactiveVals `results` to be used in other modules
#     observe({
#       req(updatedMatrixDF())  # Ensure the matrix has been updated
#       
#       # Generate the deparsed expression and store it in `results`
#       results$expressionOutput <- paste(
#         deparse(lotri::lotriAsExpression(updatedMatrixDF())), 
#         collapse = "\n"
#       )
#     })
#     
#     # Now `results$expressionOutput` can be accessed by other modules
#   })
# }

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
    
    # Render the rhandsontable
    output$triangleTable <- renderRHandsontable({
      df <- matrixDF()  # Access the reactive data frame
      
      # Set row names as column names for the table
      colnames(df) <- rownames(df)
      
      rhandsontable(df) %>%
        hot_cols(renderer = "
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
        updatedMatrix <- hot_to_r(input$triangleTable)  # Capture changes from UI
        updatedMatrixDF(updatedMatrix)  # Update the reactiveVal with the modified matrix
      }
    })
    
    # Store the expression in reactiveVals `results` to be used in other modules
    observe({
      req(updatedMatrixDF())  # Ensure the matrix has been updated
      
      # Generate the deparsed expression and store it in `results`
      results$expressionOutput <- paste(
        deparse(lotri::lotriAsExpression(updatedMatrixDF())), 
        collapse = "\n"
      )
    })
    
  })
}


