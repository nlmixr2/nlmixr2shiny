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

    # Call the PKPD model server
    pkServer("pkpdModel", results)

    # Call the Model Property server
    pkprServer("modelProperty", results)

    # Call the Parameter Estimate server
    ParEstServer("parameterEstimate", results)

    # Monitor active tab and update results$pkpdm when "Model Property" is active
    observeEvent(input$mainTabs, {
      tab <- input$mainTabs

      if (tab == "Model Property") {
        # Check if pkpdpipe is non-empty and update pkpdm


          # if (!is.null(results$pkpdpipe) && nzchar(results$pkpdpipe)) {

          # Retrieve the stored PKPD inputs from results
          pk_values <- list(
            absorption_method = results$absorption_method,
            distribution_model = results$distribution_model,
            elimination_method = results$elimination_method,
            parameterization = results$parameterization
          )

          if (results$absorption_method == "Transit") {
            pk_values$transit_compartment <- results$transit_compartment
          }

          pd_values <- list(
            response_type = results$response_type,
            drug_action = results$drug_action
          )

          if (results$response_type %in% c("Direct/Immediate", "Effect Compartment")) {
            pd_values$baseline <- results$baseline
          } else if (results$response_type == "Indirect/Turnover") {
            pd_values$type_of_model <- results$type_of_model
          }

          if (length(results$drug_action) == 1 && results$drug_action %in% c("Emax", "Imax")) {
            pd_values$sigmoidicity <- results$sigmoidicity
          }
          if (length(results$response_type) == 1 && results$response_type == "Indirect/Turnover") {
            pd_values$par_bas <- results$par_bas
          }

          # # Compute the PK and PD models
           pk_output <- ifelse(results$pk_switch, do.call(PKph, pk_values), "")
           pd_output <- ifelse(results$pd_switch, do.call(PDph, pd_values),"")
           pkpdmod <- ifelse(results$pk_switch || results$pd_switch, jPh(pk_output, pd_output), "")
          results$pkpdpipe <- pkpdmod

           #Evaluate the pipeline to get results$pkpdm
           results$pkpdm <- eval(str2lang(results$pkpdpipe))


        # } else {
        #   # If the pkpdpipe is empty, reset pkpdm
        #   results$pkpdm <- list(state = character(0))
        # }
      }

      # Reset pkpdm and modProp when switching to PKPD Model tab
      if (tab == "PKPD Model") {
        results$modProp <- NULL
        results$pkpdm <- NULL
      } else if (tab == "Model Property") {
        results$parEst <- NULL
      }
    })
  }



  # Create the Shiny app
  shinyApp(ui = ui, server = server)
}







# observeEvent(input$tabsetPanelID,{
#   if(input$tabsetPanelID == ""){
#     updateSelectizeInput(
#       session,
#       "selected",
#       choices = slowDatabaseQuery(),
#       selected = "C",
#       server = TRUE
#     )
#   }
# })



















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
