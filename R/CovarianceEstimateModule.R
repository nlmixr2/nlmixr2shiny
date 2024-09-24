
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

covServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create a reactive expression to hold the matrix data
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
  })
}
