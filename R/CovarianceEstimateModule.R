
covUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("\u03a9"),
    rHandsontableOutput(ns("triangleTable")) # Use namespace for module output
  )
}



covServer <- function(id, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create a sample matrix and convert to data frame
    matrixData <- lotri(a+b+c ~ c(1, 0, 1, 0, 0, 1))
    matrixDF <- as.data.frame(matrixData)

    # Render the rhandsontable
    output$triangleTable <- renderRHandsontable({
      colnames(matrixDF) <- rownames(matrixDF)  # Use row names as column names

      rhandsontable(matrixDF) %>%
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

