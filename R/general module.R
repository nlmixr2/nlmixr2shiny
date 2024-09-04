# pipEnv <- new.env(parent = emptyenv())
# pipEnv$step1 <- NULL

genUi <- function() {
  ui <- fluidPage(
    useShinyjs(),
    titlePanel("nlmixr2Shiny"),
    tabsetPanel(
      id = "mainTabs",
      tabPanel("PKPD Model", pkUI("pkpdModel")),
      tabPanel("Model Property", pkprUI("modelProperty")),
      tabPanel("Parameter Estimate", ParEstUI("parameterEstimate"))
      # Uncomment and add more tabs as needed
      # tabPanel("Validation", pkvalUI("validation")),
      # tabPanel("Simulation", pksimUI("simulation"))
    )
  )

  server <- function(input, output, session) {
    results <- reactiveValues(pkpdpipe = character(0), pkpdm = NULL, modProp = NULL, parEst = NULL)

    # Call each module server
    pkServer("pkpdModel", results)
    pkprServer("modelProperty", results)
    ParEstServer("parameterEstimate", results)

    observeEvent(input$mainTabs, {
      tab <- input$mainTabs

      # Handle backward navigation
      if (tab == "PKPD Model") {
        results$modProp <- NULL
      } else if (tab == "Model Property") {
        results$parEst <- NULL
      }
    })
  }

  # Create the Shiny app
  shinyApp(ui = ui, server = server)
}




















# genUi <- function() {
#   ui <- fluidPage(
#     useShinyjs(),  # Include shinyjs
#     navbarPage(" ",
#                tabPanel("PK Model", pkUI("pk")),
#                tabPanel("Model Property", pkprUI("pkpr")),
#                tabPanel("Name", textInput("modelName", "Model Name")),
#                tabPanel("Run Model",
#                         fluidRow(
#                           column(6, actionButton("runModel", "Run Model")),
#                           column(6, actionButton("resetModel", "Reset"))
#                         ),
#                         fluidRow(
#                           column(12, verbatimTextOutput("modelOutput"))
#                         )
#                )
#     )
#   )
#
#   server <- function(input, output, session) {
#     modelData <- reactiveValues()
#
#     # Initialize the PK and PD servers
#     pkModel <- callModule(pkServer, "pk")
#
#     # Store pkpdmod in reactiveValues to pass to pkprServer
#     observe({
#       modelData$pkpdmod <- pkModel()$pkpdmod
#     })
#
#     # Initialize the Model Property server with pkpdmod
#     callModule(pkprServer, "pkpr", pkpdm = modelData$pkpdmod)
#
#     # Reset all modules and clear model data when reset button is clicked
#     observeEvent(input$resetModel, {
#       reset("pk")
#       reset("pkpr")
#       updateTextInput(session, "modelName", value = "")
#       modelData$final <- NULL
#       output$modelOutput <- renderPrint({ "Model reset." })
#     })
#
#     # Run the model when the run button is clicked
#     observeEvent(input$runModel, {
#       output$modelOutput <- renderPrint({
#         if (input$modelName == "") {
#           "Please enter a model name."
#         } else {
#           # Combine or process the data from the pk, pd, and pkpr models
#           paste("Running model:", input$modelName)
#           # Placeholder for actual model running code
#         }
#       })
#     })
#   }
#
#   shinyApp(ui = ui, server = server)
# }
