# # Define the UI for the covariance module
# covUI <- function(id) {
#   ns <- NS(id)
#   fluidPage(
#     titlePanel("\u03a9"),
#     fluidRow(
#       column(
#         width = 12,
#         rHandsontableOutput(ns("triangleTable")) # Covariance table
#       )
#     ),
#     fluidRow(
#       column(
#         width = 12,
#         actionButton(ns("copyButton"), "Copy to Clipboard", icon = icon("clipboard")), # Copy button above the text area
#         textAreaInput(ns("expressionOutput"), "Expression Output", "", rows = 5, width = "100%") # Text area for expression output
#       )
#     )
#   )
# }
# 
# 
# # Define the Server logic for the covariance module
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
#     # Render the rhandsontable (covariance matrix)
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
#       results$expressionOutput <- paste(deparse(as.function(results$forCov)), collapse="\n")
#     })
#     
#     # Update the text area with the content of results$expressionOutput
#     observe({
#       req(results$expressionOutput)  # Ensure the expression is available
#       
#       updateTextAreaInput(session, "expressionOutput", value = results$expressionOutput)
#     })
#     
#     # JavaScript to handle the copy-to-clipboard functionality
#     observeEvent(input$copyButton, {
#       session$sendCustomMessage(type = "copyToClipboard", message = list(text = input$expressionOutput))
#     })
#   })
# }
# 
# # Add JavaScript for clipboard functionality in the Shiny app's `ui`
# tags$script(HTML("
#   Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
#     var text = message.text;
#     navigator.clipboard.writeText(text).then(function() {
#       alert('Copied to clipboard');
#     });
#   });
# "))


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
        rHandsontableOutput(ns("triangleTable")), # Covariance table below the button
        textAreaInput(ns("expressionOutput"), "Expression Output", "", rows = 5, width = "100%") # Text area for expression output
      )
    ),
    fluidRow(
      column(12, actionButton(ns("copy_code"), "Copy Model Code"))  # Added the copy code button
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
      results$expressionOutput <- paste(deparse(as.function(results$forCov)), collapse="\n")
    })

    # Update the text area with the content of results$expressionOutput
    observe({
      req(results$expressionOutput)  # Ensure the expression is available

      updateTextAreaInput(session, "expressionOutput", value = results$expressionOutput)
    })

    # JavaScript to handle the copy-to-clipboard functionality
    observeEvent(input$copy_code, {
      
      waiter_show(html = tagList(
        spin_fading_circles(),
        h4("Calculating, please wait...")
      ))
      session$sendCustomMessage(type = "copyToClipboard", message = list(text = input$expressionOutput))

      # Use rstudioapi's insertText to display the output in the RStudio console
      if (rstudioapi::isAvailable()) {
        rstudioapi::insertText(text = paste("mod1 <- ",results$expressionOutput))
        
        waiter_hide()
        
        shiny::stopApp()
      }
    })
  })
}

# Add JavaScript for clipboard functionality in the Shiny app's `ui`
tags$script(HTML("
  Shiny.addCustomMessageHandler('copyToClipboard', function(message) {
    var text = message.text;
    navigator.clipboard.writeText(text).then(function() {
      alert('Copied to clipboard');
    });
  });
"))